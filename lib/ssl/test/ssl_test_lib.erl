%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-module(ssl_test_lib).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ssl/src/tls_handshake_1_3.hrl").
-include_lib("ssl/src/ssl_cipher.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/ssl_record.hrl").

-export([clean_start/0,
         clean_start/1,
         clean_env/0,
         init_per_suite/2, end_per_suite/1,
         init_per_group/2,
         init_per_group_openssl/2,
         end_per_group/2,
         ct_log_supported_protocol_versions/1,
         ssl_options/2,
         ssl_options/3,
         run_where/1,
         run_where/2,
         inet_port/1,
         default_tls_version/1,
         check_sane_openssl_renegotiate/2,
         check_openssl_npn_support/1,
         check_sane_openssl_dsa/1,
         start_server/1,
         start_server/2,
         start_client/1,
         start_client/2,
         start_server/3,
         start_client/3,
         start_client/4,
         start_upgrade_server/1,
         start_upgrade_server_error/1,
         start_upgrade_client/1,
         start_upgrade_client_error/1,
         start_client_error/1,
         start_server_error/1,
         start_server_transport_abuse_socket/1,
         start_server_transport_control/1,
         start_server_with_raw_key/3,
         start_openssl_client/2,
         run_server/1,
         run_server/3,
         run_server_error/1,
         ecc_test/6,
         ecc_test_error/5,
         transport_accept_abuse/1,
         transport_switch_control/1,
         init_openssl_server/3,
         init_openssl_client/1,
         run_client_init/1,
         run_upgrade_server/1,
         run_upgrade_client/1,
         run_upgrade_server_error/1,
         run_upgrade_client_error/1,
         run_client_error/1,
         send_recv_result_active/3,
         wait_for_result/2,
         wait_for_result/4,
         wait_for_openssl_server/2,
         send_recv_result/1,
         send_recv_result_active/1,
         send_recv_result_active/2,
         send_recv_result_active_once/1,
         active_recv/2,
         active_recv_loop/3,
         active_once_recv/2,
         recv_disregard/2,
         active_disregard/2,
         active_once_disregard/2,
         send/2,
         close/1,
         close/2,
         check_active_receive/2,
         check_client_alert/2,
         check_client_alert/3,
         check_server_alert/2,
         check_server_alert/3,
         check_tickets/1,
         check_ecc/3,
         check_key_exchange_send_active/2,
         verify_active_session_resumption/2,
         verify_active_session_resumption/3,
         verify_active_session_resumption/4,
         verify_active_session_resumption/5,
         verify_server_early_data/3,
         verify_session_ticket_extension/2,
         update_session_ticket_extension/2,
         check_sane_openssl_version/2,
         check_ok/1,
         check_result/4,
         check_result/2,
         get_result/1,
         gen_check_result/4,
         basic_alert/4,
         session_id/1,
         update_keys/2,
         sanity_check/2,
         supported_eccs/1,
         no_result/1,
         receive_tickets/1,
         set_protocol_versions/1,
         user_lookup/3,
         digest/0,
         accepters/1,
         client_msg/2,
         server_msg/2,
         hardcode_rsa_key/1,
         hardcode_dsa_key/1,
         bigger_buffers/0,
         stop/2,
         working_openssl_client/1,
         hostname_format/1
        ]).

-export([basic_test/3,
         erlang_ssl_receive_and_assert_negotiated_protocol/3,
         cipher_result/2,
         assert_mfl/2,
         trigger_renegotiate/4,
         trigger_renegotiate/2,
         session_info_result/1,
         reuse_session/3,
         test_ciphers/3,
         test_cipher/2,
         openssl_ciphers/0,
         openssl_support_rsa_kex/0
        ]).

-export([tls_version/1,
         n_version/1,
         is_protocol_version/1,
         is_tls_version/1,
         is_dtls_version/1,
         protocol_version/1,
         protocol_version/2,
         protocol_options/2,
         public_key/1,
         state/1,
         new_config/2,
         node_to_hostip/2
       ]).

-export([make_rsa_cert/1,
         make_rsa_cert_with_protected_keyfile/2,
         make_dsa_cert/1,
         make_ecdsa_cert/1,
         make_ecdh_rsa_cert/1,
         make_rsa_ecdsa_cert/2,
         make_rsa_cert_chains/3,
         make_dsa_cert_chains/3,
         make_ecc_cert_chains/3,
         make_cert_chains_der/2,
         make_cert_chains_pem/4,
         make_ec_cert_chains/4,
         make_ec_cert_chains/5,
         make_rsa_1024_cert/1,
         make_rsa_pss_pem/4,
         make_rsa_sni_configs/0,
         gen_conf/4,
         make_mix_cert/1,
         default_cert_chain_conf/0,
         cert_options/1,
         rsa_non_signed_suites/1,
         dsa_suites/1,
         ecdh_dh_anonymous_suites/1,
         der_to_pem/2,
         pem_to_der/1,
         appropriate_sha/1,
         format_certs/1,
         format_cert/1,
         ecdsa_conf/0,
         eddsa_conf/0,
         default_ecc_cert_chain_conf/1,
         sig_algs/2,
         all_sig_algs/0,
         all_1_3_sig_algs/0,
         all_1_2_sig_algs/0
        ]).

-export([maybe_force_ipv4/1,
         openssl_sane_dtls/0,
         kill_openssl/0,
         openssl_allows_server_renegotiate/1,
         openssl_maxfraglen_support/0,
         is_sane_oppenssl_pss/1,
         consume_port_exit/1,
         is_sane_oppenssl_client/0,
         openssl_sane_dtls_session_reuse/0,
         sufficient_crypto_support/1,
         openssl_sane_dtls_alpn/0,
         openssl_ecdsa_suites/0,
         openssl_dsa_suites/0,
         enough_openssl_crl_support/1,
         openssl_ocsp_support/1,
         openssl_allows_client_renegotiate/1,
         version_flag/1,
         portable_cmd/2,
         portable_open_port/2,
         close_port/1,
         verify_early_data/1,
         trace/0,
         ct_pal_file/1
        ]).
%% Tracing
-export([handle_trace/3]).

-export([ktls_os/0,
         ktls_set_ulp/2,
         ktls_set_cipher/4]).

-record(sslsocket, { fd = nil, pid = nil}).
-define(SLEEP, 1000).
-define(DEFAULT_CURVE, secp256r1).
-define(PRINT_DEPTH, 100).
-define(DTLS_RECBUF, 32768).

%%====================================================================
%% API
%%====================================================================
start_client(erlang, Options, Config) ->
    start_client(Options, Config);
start_client(openssl, Options, Config) ->
    start_openssl_client(Options, Config);
start_client(Type, _Args, _Config) ->
    {error, unsupported_client_type, Type}.

start_server(erlang, Options, Config) ->
    start_server(Options, Config);
start_server(openssl, Options, Config) ->
    start_openssl_server(openssl, Options, Config);
start_server(openssl_ocsp, Options, Config) ->
    start_openssl_server(openssl_ocsp, Options, Config);
start_server(openssl_ocsp_revoked, Options, Config) ->
    start_openssl_server(openssl_ocsp_revoked, Options, Config);
start_server(openssl_ocsp_undetermined, Options, Config) ->
    start_openssl_server(openssl_ocsp_undetermined, Options, Config);
start_server(Type, _Args, _Config) ->
    {error, unsupported_server_type, Type}.


%% Test
send_recv_result_active(Peer1, Peer2, Data) ->
    ok = send(Peer1, Data),
    Data = check_active_receive(Peer2, Data),
    ok = send(Peer2, Data),
    Data = check_active_receive(Peer1, Data).


%% Options
get_server_opts(Config) ->
    get_server_opts(openssl, Config).
    %% DSOpts = proplists:get_value(server_ecdsa_opts, Config),
    %% SOpts = proplists:get_value(server_opts, Config, DSOpts),
    %% ssl_test_lib:ssl_options(SOpts, Config).
%%
get_server_opts(openssl, Config) ->
    DSOpts = proplists:get_value(server_ecdsa_opts, Config),
    SOpts = proplists:get_value(server_opts, Config, DSOpts),
    ssl_options(SOpts, Config);
get_server_opts(openssl_ocsp, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Cert = filename:join(PrivDir, "a.server/cert.pem"),
    Key = filename:join(PrivDir, "a.server/key.pem"),
    CACerts = filename:join(PrivDir, "a.server/cacerts.pem"),
    SOpts = [{reuseaddr, true},
             {cacertfile, CACerts},
             {certfile, Cert},
             {keyfile, Key}],
    ssl_options(SOpts, Config);
get_server_opts(openssl_ocsp_revoked, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Cert = filename:join(PrivDir, "revoked/cert.pem"),
    Key = filename:join(PrivDir, "revoked/key.pem"),
    CACerts = filename:join(PrivDir, "revoked/cacerts.pem"),
    SOpts = [{reuseaddr, true},
             {cacertfile, CACerts},
             {certfile, Cert},
             {keyfile, Key}],
    ssl_options(SOpts, Config);
get_server_opts(openssl_ocsp_undetermined, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Cert = filename:join(PrivDir, "undetermined/cert.pem"),
    Key = filename:join(PrivDir, "undetermined/key.pem"),
    CACerts = filename:join(PrivDir, "undetermined/cacerts.pem"),
    SOpts = [{reuseaddr, true},
             {cacertfile, CACerts},
             {certfile, Cert},
             {keyfile, Key}],
    ssl_options(SOpts, Config).

get_client_opts(Config) ->
    DCOpts = proplists:get_value(client_ecdsa_opts, Config),
    COpts = proplists:get_value(client_opts, Config, DCOpts),
    ssl_options(COpts, Config).


init_per_suite(Config0, Type) ->
    end_per_suite(Config0),
    try crypto:start() of
	ok ->
            clean_start(),
            ssl:clear_pem_cache(),
            case Type of
                openssl ->
                    Version = portable_cmd("openssl", ["version"]),
                    case Version of
                        "OpenSSL" ++ _ -> ok;
                        "LibreSSL" ++ _ -> ok;
                        _ -> throw({skip, "Unknown openssl version: " ++ Version})
                    end,
                    [{openssl_version, Version}|Config0];
                _ ->
                    Config0
            end
    catch _:_ ->
            throw({skip, "Crypto did not start"})
    end.

end_per_suite(_Config) ->
    application:stop(ssl),
    application:stop(crypto),
    ssl_test_lib:kill_openssl().


%% Default callback functions
init_per_group(GroupName, Config0) ->
    case proplists:get_value(openssl_version, Config0) of
        undefined ->
            ok;
        Version ->
            put(openssl_version, Version)
    end,
    case is_protocol_version(GroupName) andalso sufficient_crypto_support(GroupName) of
	true ->
            Config = clean_protocol_version(Config0),
	    [{version, GroupName}|init_protocol_version(GroupName, Config)];
	_ ->
	    case sufficient_crypto_support(GroupName) of
		true ->
		    ssl:start(),
		    Config0;
		false ->
		    {skip, "Missing crypto support"}
	    end
    end.

working_openssl_client(Config) ->
    case proplists:get_value(openssl_version, Config) of
        %% These versions of OpenSSL has a client that
        %% can not handle hello extensions. And will
        %% fail with bad packet length if they are present
        %% in ServerHello
        "OpenSSL 0.9.8h" ++ _ ->
            false;
        "OpenSSL 0.9.8k" ++ _ ->
            false;
        "OpenSSL" ++ _ ->
            true;
        "LibreSSL" ++ _ ->
            true
    end.

init_per_group_openssl(GroupName, Config0) ->
    case proplists:get_value(openssl_version, Config0) of
        undefined ->
            ok;
        Version ->
            put(openssl_version, Version)
    end,
    CryptoSupport = sufficient_crypto_support(GroupName),
    IsProtocolVersion = is_protocol_version(GroupName),
    if
	CryptoSupport andalso IsProtocolVersion ->
            Config = clean_protocol_version(Config0),
            ssl:start(),
	    case openssl_tls_version_support(GroupName, Config)
            of
		true ->
		    [{version, GroupName}|init_protocol_version(GroupName, Config)];
		false ->
		    {skip, "Missing openssl support"}
	    end;
        CryptoSupport ->
            ssl:start(),
            Config0;
        true ->
            {skip, "Missing crypto support"}
    end.

end_per_group(GroupName, Config) ->
  case is_protocol_version(GroupName) of
      true ->
          clean_protocol_version(Config);
      false ->
          Config
  end.

openssl_ocsp_support(Config) ->
    case proplists:get_value(openssl_version, Config) of
        "OpenSSL 1.1.1" ++ _Rest ->
            true;
        "OpenSSL 3" ++ _Rest ->
            true;
        _ ->
            false
    end.

openssl_ciphers() ->
    Str = portable_cmd("openssl", ["ciphers"]),
    Ciphers = string:split(string:strip(Str, right, $\n), ":", all),
    case portable_cmd("openssl", ["version"]) of
	"LibreSSL 3." ++ _ ->
            Ciphers -- ["DES-CBC3-SHA","AES128-SHA", "AES256-SHA", "RC4-SHA", "RC4-MD5"];
        _ ->
            Ciphers
    end.  

openssl_support_rsa_kex() ->
    case portable_cmd("openssl", ["version"]) of
        "OpenSSL 3." ++ _Rest ->
            false;
        "OpenSSL 1.1.1" ++ _Rest ->
            false;
        _ ->
            true
    end.

ecdsa_conf() ->
    [{key, {namedCurve, ?DEFAULT_CURVE}},
     {digest, appropriate_sha(crypto:supports())}].

eddsa_conf() ->
    [{key, {namedCurve, ed25519}}].

default_ecc_cert_chain_conf(eddsa_1_3) ->
    lists:map(fun(L) -> [{key, {namedCurve, ed25519}} | L] end, default_cert_chain_conf());
default_ecc_cert_chain_conf(_) ->
    default_cert_chain_conf().

sig_algs(rsa_pss_pss, _) ->
    [{signature_algs, [rsa_pss_pss_sha512,
                       rsa_pss_pss_sha384,
                       rsa_pss_pss_sha256]}];
sig_algs(rsa_pss_rsae, _) ->
    [{signature_algs, [rsa_pss_rsae_sha512,
                       rsa_pss_rsae_sha384,
                       rsa_pss_rsae_sha256]}];
sig_algs(rsa, Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
    [{signature_algs, [rsa_pss_rsae_sha512,
                       rsa_pss_rsae_sha384,
                       rsa_pss_rsae_sha256,
                       {sha512, rsa},
                       {sha384, rsa},
                       {sha256, rsa},
                       {sha, rsa}
                      ]}];
sig_algs(ecdsa, Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
    [{signature_algs, [
                       {sha512, ecdsa},
                       {sha384, ecdsa},
                       {sha256, ecdsa},
                       {sha, ecdsa}]}];
sig_algs(dsa, Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
    [{signature_algs, [{sha,dsa}]}];
sig_algs(_,_) ->
    [].

all_sig_algs() ->
    {signature_algs, list_1_3_sig_algs() ++  list_common_sig_algs() ++ list_1_2_sig_algs()}.

all_1_3_sig_algs() ->
    {signature_algs, list_1_3_sig_algs() ++ list_common_sig_algs()}.

all_1_2_sig_algs() ->
    {signature_algs, list_common_sig_algs() ++ list_1_2_sig_algs()}.

%%====================================================================
%% Internal functions
%%====================================================================
list_1_3_sig_algs() ->
    [
     eddsa_ed25519,
     eddsa_ed448,
     ecdsa_secp521r1_sha512,
     ecdsa_secp384r1_sha384,
     ecdsa_secp256r1_sha256
    ].

list_common_sig_algs() ->
    [
     rsa_pss_pss_sha512,
     rsa_pss_pss_sha384,
     rsa_pss_pss_sha256,
     rsa_pss_rsae_sha512,
     rsa_pss_rsae_sha384,
     rsa_pss_rsae_sha256
     ].

list_1_2_sig_algs() ->
    [
     {sha512, ecdsa},
     {sha512, rsa},
     {sha384, ecdsa},
     {sha384, rsa},
     {sha256, ecdsa},
     {sha256, rsa},
     {sha224, ecdsa},
     {sha224, rsa},
     {sha, ecdsa},
     {sha, rsa},
     {sha, dsa}
    ].

%% For now always run locally
run_where(_) ->
    ClientNode = node(),
    ServerNode = node(),
    Host = rpc:call(ServerNode, net_adm, localhost, []),
    {ClientNode, ServerNode, Host}.

run_where(_, ipv6) ->
    ClientNode = node(),
    ServerNode = node(),
    Host = rpc:call(ServerNode, net_adm, localhost, []),
    {ClientNode, ServerNode, Host}.

node_to_hostip(Node, Role) ->
    Host = rpc:call(Node, net_adm, localhost, []),
    {ok, Address} = inet:getaddr(Host, inet),
    %% Convert client addresses in 127.0.0.0/24 subnet to the atom 'localhost'.
    %% This is a workaround for testcase problems caused by the fact that
    %% inet:peername/1 and inet:getaddr/2 return different addresses when
    %% running on localhost.
    normalize_loopback(Address, Role).

normalize_loopback({127,_,_,_}, client) ->
    localhost;
normalize_loopback(Address, _) ->
    Address.


start_server(Args0, Config) ->
    {_, ServerNode, _} = run_where(Config),
    ServerOpts = get_server_opts(Config),
    TcServerOpts = proplists:get_value(options, Args0, []),
    Node = proplists:get_value(node, Args0, ServerNode),
    Port = proplists:get_value(port, Args0, 0),
    Args = [{from, self()},
            {node, Node},
            {port, Port},
            {options, ServerOpts++TcServerOpts} | Args0],
    start_server(Args).
%%
start_server(Args) ->
    Node = proplists:get_value(node, Args),
    Result = spawn_link(Node, ?MODULE, run_server, [Args]),
    receive
	{listen, up} ->
	    Result;
        {error, Error} ->
            Error
    end.

run_server(Opts) ->
    Port = proplists:get_value(port, Opts),
    Options = proplists:get_value(options, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    ?CT_LOG("~nssl:listen(~p, ~p)~n", [Port, format_options(Options)]),
    case Transport:listen(Port, Options) of
        {ok, ListenSocket} ->
            Pid ! {listen, up},
            send_selected_port(Pid, Port, ListenSocket),
            run_server(ListenSocket, Opts);
        Error ->
            Pid ! Error
    end.

run_server(ListenSocket, Opts) ->
    Accepters = proplists:get_value(accepters, Opts, 1),
    run_server(ListenSocket, Opts, Accepters). 
    
run_server(ListenSocket, Opts, 1) ->
    do_run_server(ListenSocket, connect(ListenSocket, Opts), Opts);
run_server(ListenSocket, Opts, N) ->
    Pid = proplists:get_value(from, Opts),
    Server = spawn(?MODULE, run_server, [ListenSocket, Opts, 1]),
    Pid ! {accepter, N, Server},
    run_server(ListenSocket, Opts, N-1).

do_run_server(_, {error, _} = Result, Opts)  ->
    ?CT_LOG("Server error result ~p~n", [Result]),
    Pid = proplists:get_value(from, Opts),
    Pid ! {self(), Result};
do_run_server(_, ok = Result, Opts) ->
    ?CT_LOG("Server cancel result ~p~n", [Result]),
    Pid = proplists:get_value(from, Opts),
    Pid ! {self(), Result};
do_run_server(ListenSocket, AcceptSocket, Opts) ->
    Pid = proplists:get_value(from, Opts),
    Transport = proplists:get_value(transport, Opts, ssl),
    MFA = proplists:get_value(mfa, Opts),
    case server_apply_mfa(AcceptSocket, MFA) of
	no_result_msg ->
	    ok;
	Msg ->
	    ?CT_LOG("~nServer Msg: ~p ~n", [Msg]),
            case lists:member(return_socket, Opts) of
                true -> Pid ! {self(), {Msg, AcceptSocket}};
                false -> Pid ! {self(), Msg}
            end
    end,
    do_run_server_core(ListenSocket, AcceptSocket, Opts, Transport, Pid).

server_apply_mfa(_, undefined) ->
    no_result_msg;
server_apply_mfa(AcceptSocket, {Module, Function, Args}) ->
    ?CT_LOG("~nServer: apply(~p,~p,~p)~n",
           [Module, Function, [AcceptSocket | Args]]),
    apply(Module, Function, [AcceptSocket | Args]).

client_apply_mfa(_, undefined) ->
    no_result_msg;
client_apply_mfa(AcceptSocket, {Module, Function, Args}) ->
    ?CT_LOG("~nClient: apply(~p,~p,~p)~n",
           [Module, Function, [AcceptSocket | Args]]),
    apply(Module, Function, [AcceptSocket | Args]).


do_run_server_core(ListenSocket, AcceptSocket, Opts, Transport, Pid) ->
    receive
        {data, Data} ->
            ?CT_LOG("[server] Send: ~p~n", [Data]),
            case Transport:send(AcceptSocket, Data) of
                ok ->
                    Pid ! {self(), ok};
                {error, Reason} ->
                    Pid ! {self(), Reason}
            end,
            do_run_server_core(ListenSocket, AcceptSocket, Opts, Transport, Pid);
        {active_receive, Data} ->
            case active_recv(AcceptSocket, length(Data)) of
                ReceivedData ->
                    ?CT_LOG("[server] Received: ~p~n", [Data]),
                    Pid ! {self(), ReceivedData}
            end,
            do_run_server_core(ListenSocket, AcceptSocket, Opts, Transport, Pid);
        {update_keys, Type} ->
            case ssl:update_keys(AcceptSocket, Type) of
                ok ->
                    ?CT_LOG("[server] Update keys: ~p", [Type]),
                    Pid ! {self(), ok};
                {error, Reason} ->
                    ?CT_LOG("[server] Update keys failed: ~p", [Type]),
                    Pid ! {self(), Reason}
            end,
            do_run_server_core(ListenSocket, AcceptSocket, Opts, Transport, Pid);
        get_socket ->
            Pid ! {self(), {socket, AcceptSocket}},
            do_run_server_core(ListenSocket, AcceptSocket, Opts, Transport, Pid);
	listen ->
	    run_server(ListenSocket, Opts);
	{listen, MFA} ->
	    run_server(ListenSocket, [MFA | proplists:delete(mfa, Opts)]);
	close ->
	    ?CT_LOG("~nServer closing~n", []),
	    Result = Transport:close(AcceptSocket),
	    Result1 = Transport:close(ListenSocket),
	    ?CT_LOG("~nResult ~p : ~p ~n", [Result, Result1])
    end.

%%% To enable to test with s_client -reconnect
connect(#sslsocket{} = ListenSocket, Opts) ->
    Node = proplists:get_value(node, Opts),
    ReconnectTimes =  proplists:get_value(reconnect_times, Opts, 0),
    Timeout = proplists:get_value(timeout, Opts, infinity),
    SslOpts = proplists:get_value(ssl_extra_opts, Opts, []),
    ContOpts = proplists:get_value(continue_options, Opts, []),
    AcceptSocket = connect(ListenSocket, Node, 1 + ReconnectTimes, dummy, Timeout, SslOpts, ContOpts),
    case ReconnectTimes of
	0 ->
	    AcceptSocket;
	_ ->
	  remove_close_msg(ReconnectTimes),
	  AcceptSocket
    end;
connect(ListenSocket, _Opts) ->
    ?CT_LOG("~ngen_tcp:accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    AcceptSocket.

connect(_, _, 0, AcceptSocket, _, _, _) ->
    AcceptSocket;
connect(ListenSocket, Node, _N, _, Timeout, SslOpts, cancel) ->
    ?CT_LOG("ssl:transport_accept(~P)~n", [ListenSocket, ?PRINT_DEPTH]),
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),    
    ?CT_LOG("~nssl:handshake(~p,~p,~p)~n", [AcceptSocket, format_options(SslOpts),Timeout]),

    case ssl:handshake(AcceptSocket, SslOpts, Timeout) of
	{ok, Socket0, Ext} ->
            ?CT_LOG("Ext ~p:~n", [Ext]),
            ?CT_LOG("~nssl:handshake_cancel(~p)~n", [Socket0]),
            ssl:handshake_cancel(Socket0);
        Result ->
	    ?CT_LOG("~nssl:handshake@~p ret ~p",[Node,Result]),
	    Result
    end;
connect(ListenSocket, Node, N, _, Timeout, SslOpts, [_|_] =ContOpts0) ->
    ?CT_LOG("ssl:transport_accept(~P)~n", [ListenSocket, ?PRINT_DEPTH]),
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),    
    ?CT_LOG("~nssl:handshake(~p,~p,~p)~n", [AcceptSocket, SslOpts,Timeout]),

    case ssl:handshake(AcceptSocket, SslOpts, Timeout) of
	{ok, Socket0, Ext} ->
            [_|_] = maps:get(sni, Ext),
            ?CT_LOG("Ext ~p:~n", [Ext]),
            ContOpts = case lists:keytake(want_ext, 1, ContOpts0) of
                           {value, {_, WantExt}, ContOpts1} ->
                               if is_pid(WantExt) ->
                                       WantExt ! {self(), {ext, Ext}};
                                  true ->
                                       ignore
                               end,
                               ContOpts1;
                           _ ->
                               ContOpts0
                       end,
            ?CT_LOG("~nssl:handshake_continue(~p,~p,~p)~n", [Socket0, ContOpts,Timeout]),
            case ssl:handshake_continue(Socket0, ContOpts, Timeout) of
                {ok, Socket} ->
                    connect(ListenSocket, Node, N-1, Socket, Timeout, SslOpts, ContOpts0);
                Error ->
                    ?CT_LOG("~nssl:handshake_continue@~p ret ~p",[Node,Error]),
                    Error
            end;
	Result ->
	    ?CT_LOG("~nssl:handshake@~p ret ~p",[Node,Result]),
	    Result
    end;
connect(ListenSocket, Node, N, _, Timeout, [], ContOpts) ->
    ?CT_LOG("ssl:transport_accept(~P)~n", [ListenSocket, ?PRINT_DEPTH]),
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),    
    ?CT_LOG("~nssl:handshake(~p, ~p)~n", [AcceptSocket, Timeout]),

    case ssl:handshake(AcceptSocket, Timeout) of
	{ok, Socket} ->
	    connect(ListenSocket, Node, N-1, Socket, Timeout, [], ContOpts);
	Result ->
	    ?CT_LOG("~nssl:handshake@~p ret ~p",[Node,Result]),
	    Result
    end;
connect(ListenSocket, _Node, _, _, Timeout, Opts, _) ->
    ?CT_LOG("ssl:transport_accept(~P)~n", [ListenSocket, ?PRINT_DEPTH]),
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),    
    ?CT_LOG("ssl:handshake(~p,~p, ~p)~n", [AcceptSocket, Opts, Timeout]),
    ssl:handshake(AcceptSocket, Opts, Timeout),
    AcceptSocket.


start_server_transport_abuse_socket(Args) ->
    Node = proplists:get_value(node, Args),
    Result = spawn_link(Node, ?MODULE, transport_accept_abuse, [Args]),
    receive
	{listen, up} ->
	    Result
    end.

start_server_transport_control(Args) ->
    Node = proplists:get_value(node, Args),
    Result = spawn_link(Node, ?MODULE, transport_switch_control, [Args]),
    receive
	{listen, up} ->
	    Result
    end.

transport_accept_abuse(Opts) ->
    Port = proplists:get_value(port, Opts),
    Options = proplists:get_value(options, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    ?CT_LOG("~nssl:listen(~p, ~p)~n", [Port, Options]),
    {ok, ListenSocket} = Transport:listen(Port, Options),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),    
    {error, _} = ssl:connection_information(AcceptSocket),
    _ = ssl:handshake(AcceptSocket, infinity),
    Pid ! {self(), ok}.

transport_switch_control(Opts) ->
    Port = proplists:get_value(port, Opts),
    Options = proplists:get_value(options, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    ?CT_LOG("~nssl:listen(~p, ~p)~n", [Port, Options]),
    {ok, ListenSocket} = Transport:listen(Port, Options),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),    
    ok = ssl:controlling_process(AcceptSocket, self()),
    Pid ! {self(), ok}.


remove_close_msg(0) ->
    ok;
remove_close_msg(ReconnectTimes) ->
    receive
	{ssl_closed, _} ->
	   remove_close_msg(ReconnectTimes -1)
    end.


start_openssl_server(Mode, Args0, Config) ->
    {_, ServerNode, _} = run_where(Config),
    ServerOpts = get_server_opts(Mode, Config),
    Node = proplists:get_value(node, Args0, ServerNode),
    Port = proplists:get_value(port, Args0, 0),
    ResponderPort = proplists:get_value(responder_port, Config, 0),
    PrivDir = proplists:get_value(priv_dir, Config),
    Args = [{from, self()}, {port, Port}] ++ ServerOpts ++ Args0 ++ [{priv_dir, PrivDir}],
    Result = spawn_link(Node, ?MODULE, init_openssl_server,
                        [Mode, ResponderPort,lists:delete(return_port, Args)]),
    receive
	{started, OpenSSLPort} ->
	    case lists:member(return_port, Args) of
		true -> {Result, OpenSSLPort};
		false -> Result
	    end;
	{start_failed, Reason} ->
	    {start_failed, Reason}
    end.

init_openssl_server(openssl, _, Options) ->
    DefaultVersions = default_tls_version(Options),
    [Version | _] = proplists:get_value(versions, Options, DefaultVersions),
    DOpenssl = proplists:get_value(debug_openssl, Options, false),
    Port = inet_port(node()),
    Pid = proplists:get_value(from, Options),

    case proplists:get_value(openssl_version, Options) of
        undefined -> ok;
        Version -> put(openssl_version, Version)
    end,

    Exe = "openssl",
    Ciphers = proplists:get_value(ciphers, Options, default_ciphers(Version)),
    Groups0 = proplists:get_value(groups, Options),
    EarlyData = proplists:get_value(early_data, Options, undefined),
    PrivDir = proplists:get_value(priv_dir, Options),
    CertArgs = openssl_cert_options(Options, server), 
    AlpnArgs = openssl_alpn_options(proplists:get_value(alpn, Options, undefined)),
    NpnArgs =  openssl_npn_options(proplists:get_value(np, Options, undefined)),                
    Debug = openssl_debug_options(PrivDir, DOpenssl),

    Args0 =  case Groups0 of
                undefined ->
                    ["s_server", "-accept", integer_to_list(Port), cipher_flag(Version),
                     ciphers(Ciphers, Version), 
                     version_flag(Version)] ++ AlpnArgs ++ NpnArgs ++ CertArgs ++ Debug;
                Group ->
                       ["s_server", "-accept", integer_to_list(Port), cipher_flag(Version),
                        ciphers(Ciphers, Version), "-groups", Group,
                        version_flag(Version)] ++ AlpnArgs ++ NpnArgs ++ CertArgs ++ Debug
            end,
    Args1 = case EarlyData of
               undefined ->
                   Args0;
               MaxSize ->
                   Args0 ++ ["-early_data", "-no_anti_replay", "-max_early_data",
                             integer_to_list(MaxSize)]
           end,
    Args = maybe_force_ipv4(Args1),
    SslPort = portable_open_port(Exe, Args),
    wait_for_openssl_server(Port, proplists:get_value(protocol, Options, tls)),
    Pid ! {started, SslPort},
    Pid ! {self(), {port, Port}},
    openssl_server_loop(Pid, SslPort, Args);

init_openssl_server(Mode, ResponderPort, Options) when Mode == openssl_ocsp orelse
                                                       Mode == openssl_ocsp_revoked orelse
                                                       Mode == openssl_ocsp_undetermined ->
    DefaultVersions = default_tls_version(Options),
    [Version | _] = proplists:get_value(versions, Options, DefaultVersions),
    Port = inet_port(node()),
    Pid = proplists:get_value(from, Options),
    GroupName = proplists:get_value(group, Options),

    Exe = "openssl",
    Ciphers = proplists:get_value(ciphers, Options, ssl:cipher_suites(default,Version)),
    CertArgs = openssl_cert_options(Options, server),
    Exe = "openssl",

    Args = ["s_server", "-accept", integer_to_list(Port), cipher_flag(Version),
            ciphers(Ciphers, Version),
            "-status_verbose",
            "-status_url",
            "http://127.0.0.1:" ++ erlang:integer_to_list(ResponderPort),
            version_flag(Version)] ++ CertArgs 
            ++ openssl_dtls_opt(GroupName),

    SslPort = portable_open_port(Exe, Args),
    wait_for_openssl_server(Port, proplists:get_value(protocol, Options, tls)),
    Pid ! {started, Port},
    Pid ! {self(), {port, Port}},
    openssl_server_loop(Pid, SslPort, Args).

openssl_dtls_opt('dtlsv1.2') ->
    ["-dtls"];
openssl_dtls_opt(_Other) ->
    [].

openssl_server_loop(Pid, SslPort, Args) ->
    receive
        {data, Data} ->
            case port_command(SslPort, Data, [nosuspend]) of
                true ->
                    ?CT_LOG("[openssl server] Send data: ~p~n", [Data]),
                    Pid ! {self(), ok};
                _Else ->
                    ?CT_LOG("[openssl server] Send failed, data: ~p~n", [Data]),
                    Pid ! {self(), {error, port_command_failed}}
            end,
            openssl_server_loop(Pid, SslPort, Args);
        {active_receive, Data} ->
            case active_recv(SslPort, length(Data)) of
                ReceivedData ->
                    ?CT_LOG("[openssl server] Received: ~p~n", [Data]),
                    Pid ! {self(), ReceivedData}
            end,
            openssl_server_loop(Pid, SslPort, Args);
        {update_keys, Type} ->
            case Type of
                write ->
                    ?CT_LOG("[openssl server] Update keys: ~p", [Type]),
                    true = port_command(SslPort, "k", [nosuspend]),
                    Pid ! {self(), ok};
                read_write ->
                    ?CT_LOG("[openssl server] Update keys: ~p", [Type]),
                    true = port_command(SslPort, "K", [nosuspend]),
                    Pid ! {self(), ok}
            end,
            openssl_server_loop(Pid, SslPort, Args);
        close ->
            ?CT_LOG("~n[openssl server] Server closing~n", []),
            catch port_close(SslPort);
        {ssl_closed, _Socket} ->
            %% TODO
            ok
    end.

start_openssl_client(Args0, Config) ->
    {ClientNode, _, Hostname} = run_where(Config),

    %% io:format("~p:~p: ~p~n",[?MODULE, ?LINE, Args0]),
    %% io:format("~p:~p: ~p~n",[?MODULE, ?LINE, Config]),

    ClientOpts0 = get_client_opts(Config),
    ClientOpts = proplists:get_value(options, Args0, []) ++ ClientOpts0,
    DefaultVersions = default_tls_version(ClientOpts),
    [Version | _] = proplists:get_value(versions, ClientOpts, DefaultVersions),
    Node = proplists:get_value(node, Args0, ClientNode),
    Args = [{from, self()}, {host, Hostname} | ClientOpts ++ Args0],

    Result = spawn_link(Node, ?MODULE, init_openssl_client,
                        [[{version, Version} | lists:delete(return_port, Args)]]),
    receive
	{connected, OpenSSLPort} ->
	    case lists:member(return_port, Args) of
		true -> {Result, OpenSSLPort};
		false -> Result
	    end;
	{connect_failed, Reason} ->
	    {connect_failed, Reason}
    end.

init_openssl_client(Options) ->
    Version = proplists:get_value(version, Options),
    Port = proplists:get_value(port, Options),
    Pid = proplists:get_value(from, Options),
    SslPort = start_client(openssl, Port, Options, [{version, Version}]),
    case proplists:get_value(openssl_version, Options) of
        undefined -> ok;
        Version -> put(openssl_version, Version)
    end,

    openssl_client_loop(Pid, SslPort, []).


openssl_client_loop(Pid, SslPort, Args) ->
    Pid ! {connected, SslPort},
    openssl_client_loop_core(Pid, SslPort, Args).

openssl_client_loop_core(Pid, SslPort, Args) ->
    receive
        {data, Data} ->
            case port_command(SslPort, Data, [nosuspend]) of
                true ->
                    ?CT_LOG("[openssl client] Send data: ~p~n", [Data]),
                    Pid ! {self(), ok};
                _Else ->
                    ?CT_LOG("[openssl client] Send failed, data: ~p~n", [Data]),
                    Pid ! {self(), {error, port_command_failed}}
            end,
            openssl_client_loop_core(Pid, SslPort, Args);
        {active_receive, Data} ->
            case active_recv(SslPort, length(Data)) of
                ReceivedData ->
                    ?CT_LOG("[openssl client] Received: ~p~n   (forward to PID=~p)~n",
                           [Data, Pid]),
                    Pid ! {self(), ReceivedData}
            end,
            openssl_client_loop_core(Pid, SslPort, Args);
        {update_keys, Type} ->
            case Type of
                write ->
                    ?CT_LOG("[openssl client] Update keys: ~p", [Type]),
                    true = port_command(SslPort, "k", [nosuspend]),
                    Pid ! {self(), ok};
                read_write ->
                    ?CT_LOG("[openssl client] Update keys: ~p", [Type]),
                    true = port_command(SslPort, "K", [nosuspend]),
                    Pid ! {self(), ok}
            end,
            openssl_client_loop_core(Pid, SslPort, Args);
        close ->
            ?CT_LOG("~nClient closing~n", []),
            catch port_close(SslPort);
        {ssl_closed, _Socket} ->
            %% TODO
            ok
    end.

start_client(Args0, Config) ->
    {_, ServerNode, Hostname} = run_where(Config),
    ClientOpts = get_client_opts(Config),
    ClientOpts1 = proplists:get_value(options, Args0, []),
    Node = proplists:get_value(node, Args0, ServerNode),
    Args1 = proplists:delete(options, Args0),
    Args = [{from, self()},
            {host, Hostname},
            {node, Node},
            {options, ClientOpts ++ ClientOpts1} | Args1],
    start_client(Args).
%%
start_client(Args) ->
    Node = proplists:get_value(node, Args),
    Result = spawn_link(Node, ?MODULE, run_client_init, [lists:delete(return_socket, Args)]),
    receive 
	{connected, Socket} ->
	    case lists:member(return_socket, Args) of
		true -> {Result, Socket};
		false -> Result
	    end;
	{connect_failed, Reason} ->
	    {connect_failed, Reason}
    end.

run_client_init(Opts) ->
    put(retries, 0),
    run_client(Opts).

run_client(Opts) ->
    Node = proplists:get_value(node, Opts),
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    Options0 = proplists:get_value(options, Opts),
    Options = patch_dtls_options(Options0),
    ContOpts = proplists:get_value(continue_options, Opts, []),
    ?CT_LOG("~n~p:connect(~p, ~p)@~p~n", [Transport, Host, Port, Node]),
    ?CT_LOG("SSLOpts:~n ~0.p", [format_options(Options)]),
    case ContOpts of
        [] ->
            client_loop(Node, Host, Port, Pid, Transport, Options, Opts);
        _ ->
            client_cont_loop(Node, Host, Port, Pid, Transport, Options, ContOpts, Opts)
    end.

client_loop(_Node, Host, Port, Pid, Transport, Options, Opts) ->
    case Transport:connect(Host, Port, Options) of
	{ok, Socket} ->
	    Pid ! {connected, Socket},
	    ?CT_LOG("~nClient: connected~n", []),
	    %% In special cases we want to know the client port, it will
	    %% be indicated by sending {port, 0} in options list!
	    send_selected_port(Pid,  proplists:get_value(port, Options), Socket),
	    MFA = proplists:get_value(mfa, Opts),
	    case client_apply_mfa(Socket, MFA) of
		no_result_msg ->
		    ok;
		Msg ->
		    ?CT_LOG("~nClient Msg: ~p ~n", [Msg]),
		    Pid ! {self(), Msg}
	    end,
            client_loop_core(Socket, Pid, Transport);
	{error, econnrefused = Reason} ->
            case proplists:get_value(return_error, Opts, undefined) of
                econnrefused ->
                    Pid ! {connect_failed, Reason};
                _ ->
                    case get(retries) of
                        N when N < 5 ->
                            ?CT_LOG("~neconnrefused retries=~p sleep ~p",[N,?SLEEP]),
                            put(retries, N+1),
                            ct:sleep(?SLEEP),
                            run_client(Opts);
                        _ ->
                            ?CT_LOG("~nClient failed several times: connection failed: ~p ~n", [Reason]),
                            Pid ! {self(), {error, Reason}}
                    end
            end;
	{error, econnreset = Reason} ->
	      case get(retries) of
		N when N < 5 ->
		    ?CT_LOG("~neconnreset retries=~p sleep ~p",[N,?SLEEP]),
		    put(retries, N+1),
		    ct:sleep(?SLEEP),
		    run_client(Opts);
	       _ ->
		    ?CT_LOG("~nClient failed several times: connection failed: ~p ~n", [Reason]),
		    Pid ! {self(), {error, Reason}}
	    end;
	{error, Reason} ->
	    ?CT_LOG("~nClient: connection failed: ~p ~n", [Reason]),
	    Pid ! {connect_failed, Reason}
    end.

client_loop_core(Socket, Pid, Transport) ->
    receive
        {data, Data} ->
            ?CT_LOG("[client] Send: ~p~n", [Data]),
            case Transport:send(Socket, Data) of
                ok ->
                    Pid ! {self(), ok};
                {error, Reason} ->
                    Pid ! {self(), Reason}
            end,
            client_loop_core(Socket, Pid, Transport);
        {active_receive, Data} ->
            case active_recv(Socket, length(Data)) of
                ReceivedData ->
                    ?CT_LOG("[client] Received: ~p~n", [Data]),
                    Pid ! {self(), ReceivedData}
            end,
            client_loop_core(Socket, Pid, Transport);
        {update_keys, Type} ->
            case ssl:update_keys(Socket, Type) of
                ok ->
                    ?CT_LOG("[client] Update keys: ~p", [Type]),
                    Pid ! {self(), ok};
                {error, Reason} ->
                    ?CT_LOG("[client] Update keys failed: ~p", [Type]),
                    Pid ! {self(), Reason}
            end,
            client_loop_core(Socket, Pid, Transport);
        get_socket ->
            Pid ! {self(), {socket, Socket}},
            client_loop_core(Socket, Pid, Transport);
        close ->
            ?CT_LOG("~nClient closing~n", []),
            Transport:close(Socket);
        {ssl_closed, Socket} ->
            ok;
        {gen_tcp, closed} ->
            ok;
        {apply, From, Fun} ->
            try Fun(Socket, Transport) of
                {replace, NewSocket} = Res ->
                    From ! {apply_res, Res},
                    client_loop_core(NewSocket, Pid, Transport);
                Res ->
                    From ! {apply_res, Res},
                    client_loop_core(Socket, Pid, Transport)
            catch E:R:ST ->
                    From ! {apply_res, {E,R,ST}},
                    client_loop_core(Socket, Pid, Transport)
            end
    end.

client_cont_loop(_Node, Host, Port, Pid, Transport, Options, cancel, _Opts) ->
    case Transport:connect(Host, Port, Options) of
        {ok, Socket, _} ->
           Result = Transport:handshake_cancel(Socket),
            ?CT_LOG("~nClient: Cancel: ~p ~n", [Result]),
            Pid ! {connect_failed, Result};
        {error, Reason} ->
	    ?CT_LOG("~nClient: connection failed: ~p ~n", [Reason]),
	    Pid ! {connect_failed, Reason}
    end;

client_cont_loop(_Node, Host, Port, Pid, Transport, Options, ContOpts0, Opts) ->
    case Transport:connect(Host, Port, Options) of
        {ok, Socket0, Ext} ->
            ContOpts = case lists:keytake(want_ext, 1, ContOpts0) of
                           {value, {_, WantExt}, ContOpts1} ->
                               if is_pid(WantExt) ->
                                       WantExt ! {self(), {ext, Ext}};
                                  true ->
                                       ignore
                               end,
                               ContOpts1;
                           _ ->
                               ContOpts0
                       end,
            ?CT_LOG("~nClient: handshake_continue(~p, ~p, infinity) ~n", [Socket0, ContOpts]),
            case Transport:handshake_continue(Socket0, ContOpts) of
                {ok, Socket} ->
                    Pid ! {connected, Socket},
                    MFA = proplists:get_value(mfa, Opts),
                    ?CT_LOG(
                       "~nClient: client_apply_mfa(~p,~p)~n", [Socket, MFA]),
                    case client_apply_mfa(Socket, MFA) of
                        no_result_msg ->
                            ok;
                        Msg ->
                            ?CT_LOG("~nClient Msg: ~p ~n", [Msg]),
                            Pid ! {self(), Msg}
                    end
	    end;
        {error, Reason} ->
            ?CT_LOG("~nClient: connection failed: ~p ~n", [Reason]),
            Pid ! {connect_failed, Reason}
    end.
            
close(Pid) ->
    ?CT_LOG("~nClose ~p ~n", [Pid]),
    Monitor = erlang:monitor(process, Pid),
    Pid ! close,
    receive
	{'DOWN', Monitor, process, Pid, Reason} ->
	    erlang:demonitor(Monitor),
	    ?CT_LOG("~nPid: ~p down due to:~p ~n", [Pid, Reason])
    end.

close(Pid, Timeout) ->
    ?CT_LOG("~n Close ~p ~n", [Pid]),
    Monitor = erlang:monitor(process, Pid),
    Pid ! close,
    receive
	{'DOWN', Monitor, process, Pid, Reason} ->
	    erlang:demonitor(Monitor),
	    ?CT_LOG("~nPid: ~p down due to:~p ~n", [Pid, Reason])
    after 
	Timeout ->
	    exit(Pid, kill)
    end.

get_result(Pids) ->
    get_result(Pids, []).

get_result([], Acc) -> Acc;
get_result([Pid | Tail], Acc) ->
    receive {Pid, Msg} -> get_result(Tail, [{Pid, Msg} | Acc])
    end.

check_result(Pid, Msg) ->
    check_result([{Pid, Msg}]).
check_result(Server, ServerMsg, Client, ClientMsg) ->
    check_result([{Server, ServerMsg}, {Client, ClientMsg}]).

check_result([]) -> ok;
check_result(Msgs) ->
    receive
        Msg -> match_result_msg(Msg, Msgs)
    end.

match_result_msg(Msg, Msgs) ->
    case lists:member(Msg, Msgs) of
        true  -> check_result(lists:delete(Msg, Msgs));
        false -> match_result_msg2(Msg, Msgs)
    end.

match_result_msg2({Pid, {ok, {{127,_,_,_}, Port}}} = Msg, Msgs) ->
    Match = {Pid, {ok, {localhost, Port}}},
    case lists:member(Match, Msgs) of
        true -> check_result(lists:delete(Match, Msgs));
        false -> ct:fail({{expected, Msgs}, {got, Msg}})
    end;
match_result_msg2({Port, {data,Debug}}, Msgs) when is_port(Port) ->
    ?CT_LOG(" Openssl (~p) ~s~n",[Port, Debug]),
    check_result(Msgs);
match_result_msg2({Port, closed}, Msgs) when is_port(Port) ->
    ?CT_LOG(" Openssl port (~p) closed ~n",[Port]),
    check_result(Msgs);
match_result_msg2(Msg, Msgs) ->
    ct:fail({{expected, Msgs}, {got, Msg}}).

check_server_alert(Pid, Alert) ->
    receive
	{Pid, {error, {tls_alert, {Alert, STxt}}}} ->
            check_server_txt(STxt),
            ok;
        {Pid, {error, {tls_alert, {OtherAlert, STxt}}}} ->
            ct:fail("Unexpected alert during negative test: ~p - ~p", [OtherAlert, STxt]);
        {Pid, {error, closed}} ->
            ok;
        {Pid, {ok, _}} ->
            ct:fail("Successful connection during negative test.")
    end.

check_server_alert(Server, Client, Alert) ->
    receive
	{Server, {error, {tls_alert, {Alert, STxt}}}} ->
            check_server_txt(STxt),
            check_client_alert(Client, Alert);
        {Server, {ok, _}} ->
            ct:fail("Successful connection during negative test.")
    end.

check_client_alert(Pid, Alert) ->
    receive
	{Pid, {error, {tls_alert, {Alert, CTxt}}}} ->
            check_client_txt(CTxt),
            ok;
        {Pid, {error, {tls_alert, {OtherAlert, CTxt}}}} ->
            ct:fail("Unexpected alert during negative test: ~p - ~p", [OtherAlert, CTxt]);
        {Pid, {ssl_error, _, {tls_alert, {Alert, CTxt}}}} ->
            check_client_txt(CTxt),
            ok;
        {Pid, {ssl_error, _, {tls_alert, {OtherAlert, CTxt}}}} ->
            ct:fail("Unexpected alert during negative test: ~p - ~p", [OtherAlert, CTxt]);
        {Pid, {error, closed}} ->
            ok;
        {Pid, {ok, _}} ->
            ct:fail("Successful connection during negative test.")
    end.
check_client_alert(Server, Client, Alert) ->
    receive
	{Client, {error, {tls_alert, {Alert, CTxt}}}} ->
            check_client_txt(CTxt),
            check_server_alert(Server, Alert);
        {Client, {ssl_error, _, {tls_alert, {Alert, CTxt}}}} ->
            check_client_txt(CTxt),
            ok;
        {Client, {error, closed}} ->
            ok;
        {Client, {ok, _}} ->
            ct:fail("Successful connection during negative test.")
    end.
check_server_txt("TLS server" ++ _) ->
    ok;
check_server_txt("DTLS server" ++ _) ->
    ok;
check_server_txt(Txt) ->
    ct:fail({expected_server, {got, Txt}}).

check_client_txt("TLS client" ++ _) ->
    ok;
check_client_txt("DTLS client" ++ _) ->
    ok;
check_client_txt(Txt) ->
    ct:fail({expected_server, {got, Txt}}).

wait_for_result(Server, ServerMsg, Client, ClientMsg) -> 
    receive 
	{Server, ServerMsg} -> 
	    receive 
		{Client, ClientMsg} ->
		    ok
		%% Unexpected ->
		%%     Unexpected
	    end;
	{Client, ClientMsg} -> 
	    receive 
		{Server, ServerMsg} ->
		    ok
		%% Unexpected ->
		%%     Unexpected
	    end;
	{Port, {data,Debug}} when is_port(Port) ->
	    ?CT_LOG("~nopenssl ~s~n",[Debug]),
	    wait_for_result(Server, ServerMsg, Client, ClientMsg)
	%% Unexpected ->
	%%     Unexpected
    end.

check_ok([]) ->
    ok;
check_ok(Pids) ->
    receive 
	{Pid, ok} ->
	    check_ok(lists:delete(Pid, Pids));
	Other ->
	    ct:fail({expected, {"pid()", ok}, got, Other})
    end.
	
wait_for_result(Pid, Msg) -> 
    receive 
	{Pid, Msg} -> 
	    ok;
	{Port, {data,Debug}} when is_port(Port) ->
	    ?CT_LOG("~nopenssl ~s~n",[Debug]),
	    wait_for_result(Pid,Msg)
	%% Unexpected ->
	%%     Unexpected
    end.

patch_dtls_options(Options0) ->
    case proplists:get_value(protocol, Options0) of
        dtls ->
            case proplists:get_value(recbuf, Options0, undefined) of
                undefined -> [{recbuf, ?DTLS_RECBUF}|Options0];
                _ -> Options0
            end;
        _ ->    Options0
    end.

format_options([{cacerts, Certs}|R]) ->
    [{cacerts, format_certs(Certs)} | format_options(R)];
format_options([{cert, Certs}|R]) ->
    [{cert, format_certs(Certs)} | format_options(R)];
format_options([{key, Key}|R]) ->
    [{key, lists:flatten(io_lib:format("~W",[Key, ?PRINT_DEPTH]))} | format_options(R)];
format_options([Opt|R]) ->
    [Opt | format_options(R)];
format_options([]) ->
    [].

format_certs(Certs) when is_list(Certs) ->
    [lists:flatten(format_cert(C)) || C <- Certs];
format_certs(Cert) when is_binary(Cert) ->
    lists:flatten(format_cert(Cert)).

format_cert(BinCert) when is_binary(BinCert) ->
    format_cert(public_key:pkix_decode_cert(BinCert, otp));
format_cert(#cert{otp=Otp}) ->
    format_cert(Otp);
format_cert(#'OTPCertificate'{tbsCertificate = Cert} = OtpCert) ->
    #'OTPTBSCertificate'{subject = Subject, serialNumber = Nr, issuer = Issuer} = Cert,
    case public_key:pkix_is_self_signed(OtpCert) of
        true ->
            io_lib:format("~.3w: ~s ->   selfsigned", [Nr, format_subject(Subject)]);
        false ->
            case public_key:pkix_issuer_id(OtpCert, other) of
                {ok, {IsNr, Issuer0}} ->
                    io_lib:format("~.3w:~s -> ~.3w:~s", [Nr, format_subject(Subject), IsNr, format_subject(Issuer0)]);
                {error, _} ->
                    io_lib:format("~.3w:~s ->    :~s", [Nr, format_subject(Subject), format_subject(Issuer)])
            end
    end;
format_cert(Cert) ->
    io_lib:format("Format failed for ~p", [Cert]).

format_subject({rdnSequence, Seq}) ->
    format_subject(Seq);
format_subject([[{'AttributeTypeAndValue', ?'id-at-commonName', {_, String}}]|_]) ->
    String;
format_subject([_|R]) ->
    format_subject(R);
format_subject([]) ->
    "no commonname".

cert_options(Config) ->
    ClientCaCertFile = filename:join([proplists:get_value(priv_dir, Config),
				      "client", "cacerts.pem"]),
    ClientCertFile = filename:join([proplists:get_value(priv_dir, Config),
				    "client", "cert.pem"]),
    ClientCertFileDigitalSignatureOnly = filename:join([proplists:get_value(priv_dir, Config),
                                                        "client", "digital_signature_only_cert.pem"]),
    ServerCaCertFile = filename:join([proplists:get_value(priv_dir, Config),
				      "server", "cacerts.pem"]),
    ServerCertFile = filename:join([proplists:get_value(priv_dir, Config),
				    "server", "cert.pem"]),
    ServerKeyFile = filename:join([proplists:get_value(priv_dir, Config),
                                   "server", "key.pem"]),
    ClientKeyFile = filename:join([proplists:get_value(priv_dir, Config),
                                   "client", "key.pem"]),
    ServerKeyCertFile = filename:join([proplists:get_value(priv_dir, Config),
				       "server", "keycert.pem"]),
    ClientKeyCertFile = filename:join([proplists:get_value(priv_dir, Config),
				       "client", "keycert.pem"]),

    BadCaCertFile = filename:join([proplists:get_value(priv_dir, Config),
				   "badcacert.pem"]),
    BadCertFile = filename:join([proplists:get_value(priv_dir, Config),
                                 "badcert.pem"]),
    BadKeyFile = filename:join([proplists:get_value(priv_dir, Config),
                                "badkey.pem"]),

    [{client_opts, [{cacertfile, ClientCaCertFile},
		    {certfile, ClientCertFile},
		    {keyfile, ClientKeyFile}, {verify, verify_none}]},
     {client_verification_opts, [{cacertfile, ServerCaCertFile},
                                 {certfile, ClientCertFile},
                                 {keyfile, ClientKeyFile},
                                 {verify, verify_peer}]},
     {client_verification_opts_digital_signature_only, [{cacertfile, ServerCaCertFile},
                                                        {certfile, ClientCertFileDigitalSignatureOnly},
                                                        {keyfile, ClientKeyFile},
                                                        {verify, verify_peer}]},
     {server_opts, [{reuseaddr, true}, {cacertfile, ServerCaCertFile},
                    {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {server_verification_opts, [{verify, verify_peer},{reuseaddr, true},
                                  {cacertfile, ClientCaCertFile},
                                 {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {client_kc_opts, [{certfile, ClientKeyCertFile},  {verify, verify_none}]},
     {server_kc_opts, [{reuseaddr, true},
                       {certfile, ServerKeyCertFile}]},
      {client_bad_ca, [{cacertfile, BadCaCertFile},
                       {certfile, ClientCertFile},
                       {keyfile, ClientKeyFile},
                       {verify, verify_peer}]},
     {client_bad_cert, [{cacertfile, ClientCaCertFile},
                        {certfile, BadCertFile},
                        {keyfile, ClientKeyFile},
                         {verify, verify_peer}]},
     {server_bad_ca, [{cacertfile, BadCaCertFile},
                      {certfile, ServerCertFile},
                      {keyfile, ServerKeyFile}]},
     {server_bad_cert, [{cacertfile, ServerCaCertFile},
                         {certfile, BadCertFile}, {keyfile, ServerKeyFile}]},
     {server_bad_key, [{cacertfile, ServerCaCertFile},
                       {certfile, ServerCertFile}, {keyfile, BadKeyFile}]}
    | Config].

make_dsa_cert(Config) ->  
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(dss, proplists:get_value(public_keys, CryptoSupport)) of
        true ->
            ClientChain = proplists:get_value(client_chain, Config, default_cert_chain_conf()),
            ServerChain = proplists:get_value(server_chain, Config, default_cert_chain_conf()),
            CertChainConf = gen_conf(dsa, dsa, ClientChain, ServerChain),
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "dsa"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "dsa"]),
            GenCertData = public_key:pkix_test_data(CertChainConf),
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),
            
          [{server_dsa_opts, ServerConf},
           {server_dsa_verify_opts, [{verify, verify_peer} | ServerConf]},
           {client_dsa_opts, ClientConf}
           | Config];
        false ->
          Config
  end.


make_cert_chains_der(Alg, UserConf) ->
    ClientChain = proplists:get_value(client_chain, UserConf, default_cert_chain_conf()),
    ServerChain = proplists:get_value(server_chain, UserConf, default_cert_chain_conf()),
    CertChainConf = gen_conf(Alg, Alg, ClientChain, ServerChain, curve_default(Alg)),
    public_key:pkix_test_data(CertChainConf).

make_cert_chains_pem(Alg, UserConf, Config, Suffix) ->
    ClientChain = proplists:get_value(client_chain, UserConf, default_cert_chain_conf()),
    ServerChain = proplists:get_value(server_chain, UserConf, default_cert_chain_conf()),
    CertChainConf = gen_conf(Alg, Alg, ClientChain, ServerChain),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(Alg) ++ Suffix]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(Alg) ++ Suffix]),
    GenCertData = public_key:pkix_test_data(CertChainConf),
    Conf = x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    CConf = proplists:get_value(client_config, Conf),
    SConf = proplists:get_value(server_config, Conf),
    #{server_config => SConf,
      client_config => CConf}.

make_rsa_cert_chains(UserConf, Config, Suffix) ->
    ClientChain = proplists:get_value(client_chain, UserConf, default_cert_chain_conf()),
    ServerChain = proplists:get_value(server_chain, UserConf, default_cert_chain_conf()),
    CertChainConf = gen_conf(rsa, rsa, ClientChain, ServerChain),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa" ++ Suffix]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa" ++ Suffix]),
    GenCertData = public_key:pkix_test_data(CertChainConf),
    [{server_config, ServerConf}, 
     {client_config, ClientConf}] = 
        x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    {[{verify, verify_peer} | ClientConf],
     [{reuseaddr, true}, {verify, verify_peer} | ServerConf]
    }.

make_ecc_cert_chains(UserConf, Config, Suffix) ->
    ClientChain = proplists:get_value(client_chain, UserConf, default_cert_chain_conf()),
    ServerChain = proplists:get_value(server_chain, UserConf, default_cert_chain_conf()),
    CertChainConf = gen_conf(ecdsa, ecdsa, ClientChain, ServerChain),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdsa" ++ Suffix]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdsa" ++ Suffix]),
    GenCertData = public_key:pkix_test_data(CertChainConf),
    [{server_config, ServerConf}, 
     {client_config, ClientConf}] = 
        x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    {[{verify, verify_peer} | ClientConf],
     [{reuseaddr, true}, {verify, verify_peer} | ServerConf]
    }.


make_dsa_cert_chains(UserConf, Config, Suffix) ->  
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(dss, proplists:get_value(public_keys, CryptoSupport)) of
        true ->
            ClientChain = proplists:get_value(client_chain, UserConf, default_cert_chain_conf()),
            ServerChain = proplists:get_value(server_chain, UserConf, default_cert_chain_conf()),
            CertChainConf = gen_conf(dsa, dsa, ClientChain, ServerChain),
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "dsa" ++ Suffix]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "dsa" ++ Suffix]),
            GenCertData = public_key:pkix_test_data(CertChainConf),
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),
            {[{verify, verify_peer} | ClientConf],
             [{reuseaddr, true}, {verify, verify_peer} | ServerConf]};
      false ->
          Config
  end.

make_ec_cert_chains(UserConf, ClientChainType, ServerChainType, Config) ->
    make_ec_cert_chains(UserConf, ClientChainType, ServerChainType, Config, ?DEFAULT_CURVE).
%%
make_ec_cert_chains(UserConf, ClientChainType, ServerChainType, Config, Curve) ->
    ClientChain = proplists:get_value(client_chain, UserConf, default_cert_chain_conf()),
    ServerChain = proplists:get_value(server_chain, UserConf, default_cert_chain_conf()),
    CertChainConf = gen_conf(ClientChainType, ServerChainType, ClientChain, ServerChain, Curve),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(ClientChainType)]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(ServerChainType)]),
    GenCertData = public_key:pkix_test_data(CertChainConf),
    [{server_config, ServerConf}, 
     {client_config, ClientConf}] = 
        x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    {ClientConf, [{reuseaddr, true} | ServerConf]}.

default_cert_chain_conf() ->
    %% Use only default options
    [[],[],[]].

make_rsa_pss_pem(Alg, _UserConf, Config, Suffix) ->
    DefClientConf = chain_spec(client, Alg, []),
    DefServerConf = chain_spec(server, Alg, []),
    CertChainConf = new_format([{client_chain, DefClientConf}, {server_chain, DefServerConf}]),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(Alg) ++ Suffix]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(Alg) ++ Suffix]),
    GenCertData = public_key:pkix_test_data(CertChainConf),
    Conf = x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    CConf = proplists:get_value(client_config, Conf),
    SConf = proplists:get_value(server_config, Conf),
    #{server_config => SConf,
      client_config => CConf}.

make_rsa_sni_configs() ->
    Sha = appropriate_sha(crypto:supports()),
    HostName = net_adm:localhost(),
    HostConf = make_sni_conf(HostName, Sha),
    LocalHostConf = make_sni_conf("localhost", Sha),
    {HostConf, LocalHostConf}.

make_sni_conf(HostName, Sha) ->
    public_key:pkix_test_data(#{server_chain =>
                                    #{root => [{digest, Sha},
                                               {key, hardcode_rsa_key(1)}],
                                      intermediates => [[{digest, Sha},
                                                         {key, hardcode_rsa_key(2)}]],
                                      peer => [{digest, Sha},
                                               {extensions, [#'Extension'{extnID =
                                                                              ?'id-ce-subjectAltName',
                                                                          extnValue = [{dNSName, HostName}],
                                                                          critical = false}]},
                                               {key, hardcode_rsa_key(3)}
                                              ]},
                                client_chain =>
                                    #{root => [{digest, Sha},
                                               {key, hardcode_rsa_key(4)}],
                                      intermediates => [[{digest, Sha},
                                                         {key, hardcode_rsa_key(5)}]],
                                      peer => [{digest, Sha},
                                               {key, hardcode_rsa_key(6)}]}}).

gen_conf(ClientChainType, ServerChainType, UserClient, UserServer) ->
    gen_conf(ClientChainType, ServerChainType, UserClient, UserServer, ?DEFAULT_CURVE).
%%
gen_conf(mix, mix, UserClient, UserServer, _) ->
    ClientTag = conf_tag("client"),
    ServerTag = conf_tag("server"),

    DefaultClient = default_cert_chain_conf(), 
    DefaultServer = default_cert_chain_conf(),
    
    ClientConf = merge_chain_spec(UserClient, DefaultClient, []),
    ServerConf = merge_chain_spec(UserServer, DefaultServer, []),
    
    new_format([{ClientTag, ClientConf}, {ServerTag, ServerConf}]);
gen_conf(ClientChainType, ServerChainType, UserClient, UserServer, Curve) ->
    ClientTag = conf_tag("client"),
    ServerTag = conf_tag("server"),

    DefaultClient = chain_spec(client, ClientChainType, Curve),
    DefaultServer = chain_spec(server, ServerChainType, Curve),
    
    ClientConf = merge_chain_spec(UserClient, DefaultClient, []),
    ServerConf = merge_chain_spec(UserServer, DefaultServer, []),
    
    new_format([{ClientTag, ClientConf}, {ServerTag, ServerConf}]).

new_format(Conf) ->
    CConf = proplists:get_value(client_chain, Conf),
    SConf = proplists:get_value(server_chain, Conf),
    #{server_chain => proplist_to_map(SConf),
      client_chain => proplist_to_map(CConf)}.

proplist_to_map([Head | Rest]) -> 
    [Last | Tail] = lists:reverse(Rest),
    #{root => Head,
      intermediates => lists:reverse(Tail),
      peer => Last}.

conf_tag(Role) ->
    list_to_atom(Role ++ "_chain").

chain_spec(_Role, ecdh_rsa, Curve) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    CurveOid = pubkey_cert_records:namedCurves(Curve),
     [[Digest, {key, {namedCurve, CurveOid}}],
      [Digest, {key, hardcode_rsa_key(1)}],
      [Digest, {key, {namedCurve, CurveOid}}]];

chain_spec(_Role, ecdhe_ecdsa, Curve) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    CurveOid = pubkey_cert_records:namedCurves(Curve),
    [[Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}]];

chain_spec(_Role, ecdh_ecdsa, Curve) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    CurveOid = pubkey_cert_records:namedCurves(Curve),
    [[Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}]];
chain_spec(_Role, ecdhe_rsa, _) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    [[Digest, {key, hardcode_rsa_key(1)}],
     [Digest, {key, hardcode_rsa_key(2)}],
     [Digest, {key, hardcode_rsa_key(3)}]];
chain_spec(_Role, ecdsa, Curve) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    CurveOid = pubkey_cert_records:namedCurves(Curve),
    [[Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}]];
chain_spec(_Role, eddsa, Curve) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    CurveOid = pubkey_cert_records:namedCurves(Curve),
    [[Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}],
     [Digest, {key, {namedCurve, CurveOid}}]];
chain_spec(_Role, rsa, _) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    [[Digest, {key, hardcode_rsa_key(1)}],
     [Digest, {key, hardcode_rsa_key(2)}],
     [Digest, {key, hardcode_rsa_key(3)}]];
chain_spec(_Role, 'rsa-1024', _) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    [[Digest, {key, hardcode_rsa_1024_key(1)}],
     [Digest, {key, hardcode_rsa_1024_key(2)}],
     [Digest, {key, hardcode_rsa_1024_key(3)}]];
chain_spec(client, rsa_pss_rsae, _) ->
    Digest = {digest, sha256},
    [[Digest,  {rsa_padding, rsa_pss_rsae}, {key, hardcode_rsa_key(1)}],
     [Digest,  {rsa_padding, rsa_pss_rsae}, {key, hardcode_rsa_key(2)}],
     [Digest,  {rsa_padding, rsa_pss_rsae}, {key, hardcode_rsa_key(3)}]];
chain_spec(server, rsa_pss_rsae, _) ->
    Digest = {digest, sha256},
    [[Digest,  {rsa_padding, rsa_pss_rsae}, {key, hardcode_rsa_key(4)}],
     [Digest,  {rsa_padding, rsa_pss_rsae}, {key, hardcode_rsa_key(5)}],
     [Digest,  {rsa_padding, rsa_pss_rsae}, {key, hardcode_rsa_key(6)}]];
chain_spec(client, rsa_pss_pss, _) ->
    Digest = {digest, sha256},
    [[Digest, {rsa_padding, rsa_pss_pss}, {key, {hardcode_rsa_key(1), pss_params(sha256)}}],
     [Digest, {rsa_padding, rsa_pss_pss}, {key, {hardcode_rsa_key(2), pss_params(sha256)}}],
     [Digest, {rsa_padding, rsa_pss_pss}, {key, {hardcode_rsa_key(3), pss_params(sha256)}}]];
chain_spec(server, rsa_pss_pss, _) ->
    Digest = {digest, sha256},
    [[Digest, {rsa_padding, rsa_pss_pss}, {key, {hardcode_rsa_key(4), pss_params(sha256)}}],
     [Digest, {rsa_padding, rsa_pss_pss}, {key, {hardcode_rsa_key(5),  pss_params(sha256)}}],
     [Digest, {rsa_padding, rsa_pss_pss}, {key, {hardcode_rsa_key(6),  pss_params(sha256)}}]];
chain_spec(_Role, dsa, _) ->
    Digest = {digest, appropriate_sha(crypto:supports())},
    [[Digest, {key, hardcode_dsa_key(1)}],
     [Digest, {key, hardcode_dsa_key(2)}],
     [Digest, {key, hardcode_dsa_key(3)}]].

merge_chain_spec([], [], Acc)->
    lists:reverse(Acc);
merge_chain_spec([User| UserRest], [Default | DefaultRest], Acc) ->
    Merge = merge_spec(User, Default, confs(), []),
    merge_chain_spec(UserRest, DefaultRest, [Merge | Acc]).

confs() ->
    [key, digest, validity, extensions].

merge_spec(_, _, [], Acc) ->
    Acc;
merge_spec(User, Default, [Conf | Rest], Acc) ->
    case proplists:get_value(Conf, User, undefined) of
        undefined ->
            case proplists:get_value(Conf, Default, undefined) of
                undefined ->
                    merge_spec(User, Default, Rest, Acc);
                Value  ->
                    merge_spec(User, Default, Rest, [{Conf, Value} | Acc])
            end;
        Value ->
                merge_spec(User, Default, Rest, [{Conf, Value} | Acc])
    end.

make_mix_cert(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
    Digest = {digest, appropriate_sha(crypto:supports())},
    CurveOid = pubkey_cert_records:namedCurves(?DEFAULT_CURVE),
    Mix = proplists:get_value(mix, Config, peer_ecc),
    ClientChainType =ServerChainType = mix,
    {ClientChain, ServerChain} = mix(Mix, Digest, CurveOid, Ext),
    CertChainConf = gen_conf(ClientChainType, ServerChainType, ClientChain, ServerChain),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "mix" ++ atom_to_list(Mix)]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "mix" ++ atom_to_list(Mix)]),
    GenCertData = public_key:pkix_test_data(CertChainConf),
    [{server_config, ServerConf}, 
     {client_config, ClientConf}] = 
        x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    {[{verify, verify_peer} | ClientConf],
     [{reuseaddr, true}, {verify, verify_peer} | ServerConf]
    }.

mix(peer_ecc, Digest, CurveOid, Ext) ->
    ClientChain =  [[Digest, {key, {namedCurve, CurveOid}}], 
                    [Digest, {key, hardcode_rsa_key(1)}], 
                    [Digest, {key, {namedCurve, CurveOid}}, {extensions, Ext}]
                   ],
    ServerChain =  [[Digest, {key, {namedCurve, CurveOid}}], 
                    [Digest, {key,  hardcode_rsa_key(2)}], 
                    [Digest, {key, {namedCurve, CurveOid}},{extensions, Ext}]
                   ],
    {ClientChain, ServerChain};

mix(peer_rsa, Digest, CurveOid, Ext) ->
    ClientChain =  [[Digest, {key, {namedCurve, CurveOid}}], 
                    [Digest, {key, {namedCurve, CurveOid}}], 
                    [Digest, {key, hardcode_rsa_key(1)}, {extensions, Ext}]
                   ],
    ServerChain =  [[Digest, {key, {namedCurve, CurveOid}}], 
                    [Digest, {key, {namedCurve, CurveOid}}], 
                    [Digest, {key,  hardcode_rsa_key(2)},{extensions, Ext}]
                   ],
    {ClientChain, ServerChain}.

make_ecdsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(ecdsa, proplists:get_value(public_keys, CryptoSupport)) of
        true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdsa"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdsa"]),
            ClientChain = proplists:get_value(client_chain, Config, default_cert_chain_conf()),
            ServerChain = proplists:get_value(server_chain, Config, default_cert_chain_conf()),
            CertChainConf = gen_conf(ecdsa, ecdsa, ClientChain, ServerChain),
            GenCertData = public_key:pkix_test_data(CertChainConf),
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
	    [{server_ecdsa_opts, [{reuseaddr, true} | ServerConf]},
             
	     {server_ecdsa_verify_opts, [{reuseaddr, true},
					 {verify, verify_peer} | ServerConf]},
	     {client_ecdsa_opts, [{verify, verify_none} | ClientConf]},
             {client_ecdsa_verify_opts, [{verify, verify_peer} | ClientConf]}
	     | Config];
	false ->
	    Config
    end.
make_rsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(rsa, proplists:get_value(public_keys, CryptoSupport)) of
        true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa"]),
            ClientChain = proplists:get_value(client_chain, Config, default_cert_chain_conf()),
            ServerChain = proplists:get_value(server_chain, Config, default_cert_chain_conf()),
            CertChainConf = gen_conf(rsa, rsa, ClientChain, ServerChain),
            GenCertData = public_key:pkix_test_data(CertChainConf),
            #{client_config := ClientDerConf, server_config := ServerDerConf} = GenCertData,

            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
	    [{server_rsa_opts, [{reuseaddr, true} | ServerConf]},
	     {server_rsa_verify_opts, [{reuseaddr, true}, {verify, verify_peer} | ServerConf]},
	     {client_rsa_opts, [{verify, verify_none} | ClientConf]},
             {client_rsa_verify_opts, [{verify, verify_peer} | ClientConf]},
             {server_rsa_der_opts, [{reuseaddr, true}, {verify, verify_none} | ServerDerConf]},
	     {server_rsa_der_verify_opts, [{reuseaddr, true}, {verify, verify_peer} | ServerDerConf]},
	     {client_rsa_der_opts, [{verify, verify_none} | ClientDerConf]},
             {client_rsa_der_verify_opts,  [{verify, verify_peer} |ClientDerConf]}
            | Config];
	false ->
	    Config
    end.

make_rsa_cert_with_protected_keyfile(Config0, Password) ->
    Config1 = make_rsa_cert(Config0),

    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config1),
    [PemEntry] = pem_to_der(proplists:get_value(keyfile, ClientOpts)),
    ASN1OctetStrTag = 4,
    IV = <<4,8,154,8,95,192,188,232,4,8,154,8,95,192,188,232>>,
    Length = <<16:8/unsigned-big-integer>>,
    Params = {"AES-256-CBC",
              {'PBES2-params',
               {'PBES2-params_keyDerivationFunc',
                ?'id-PBKDF2',
                {'PBKDF2-params',
                 {specified, <<125,96,67,95,2,233,224,174>>},
                 2048,asn1_NOVALUE,
                 {'PBKDF2-params_prf', ?'id-hmacWithSHA1','NULL'}}},
               {'PBES2-params_encryptionScheme',
                ?'id-aes256-CBC',
                {asn1_OPENTYPE, <<ASN1OctetStrTag, Length/binary, IV/binary>>}}}},
    ProtectedPemEntry = public_key:pem_entry_encode(
                          'PrivateKeyInfo',public_key:pem_entry_decode(PemEntry),
                          {Params, Password}),
    ProtectedClientKeyFile = filename:join(proplists:get_value(priv_dir,Config1),
                                           "tls_password_client.pem"),
    der_to_pem(ProtectedClientKeyFile, [ProtectedPemEntry]),
    ProtectedClientOpts = [{keyfile,ProtectedClientKeyFile} | proplists:delete(keyfile, ClientOpts)],
    [{client_protected_rsa_opts, ProtectedClientOpts} | Config1].

make_rsa_1024_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(rsa, proplists:get_value(public_keys, CryptoSupport)) of
        true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa-1024"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa-1024"]),
            ClientChain = proplists:get_value(client_chain, Config, default_cert_chain_conf()),
            ServerChain = proplists:get_value(server_chain, Config, default_cert_chain_conf()),
            CertChainConf = gen_conf('rsa-1024', 'rsa-1024', ClientChain, ServerChain),
            GenCertData = public_key:pkix_test_data(CertChainConf),
            #{client_config := ClientDerConf, server_config := ServerDerConf} = GenCertData,
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
	    [{server_rsa_1024_opts, [{ssl_imp, new},{reuseaddr, true} | ServerConf]},
	     {server_rsa_1024_verify_opts, [{reuseaddr, true}, {verify, verify_peer} | ServerConf]},
	     {client_rsa_1024_opts, [{verify, verify_none} | ClientConf]},
             {client_rsa_1024_verify_opts,  [{verify, verify_peer} |ClientConf]},
             {server_rsa_1024_der_opts, [{reuseaddr, true} | ServerDerConf]},
	     {server_rsa_1024_der_verify_opts, [{reuseaddr, true}, {verify, verify_peer} | ServerDerConf]},
	     {client_rsa_1024_der_opts, [{verify, verify_none} | ClientDerConf]},
             {client_rsa_1024_der_verify_opts,  [{verify, verify_peer} |ClientDerConf]}
	     | Config];
	false ->
	    Config
    end.

appropriate_sha(CryptoSupport) ->
    Hashes = proplists:get_value(hashs, CryptoSupport),
    case portable_cmd("openssl", ["version"]) of
        "OpenSSL 0.9.8" ++  _ ->
            sha;
        _ ->
            case lists:member(sha256, Hashes) of
                true ->
                    sha256;
                false ->
                    sha
            end
    end.

%% RFC 4492, Sect. 2.3.  ECDH_RSA
%%
%%    This key exchange algorithm is the same as ECDH_ECDSA except that the
%%    server's certificate MUST be signed with RSA rather than ECDSA.
make_ecdh_rsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(ecdh, proplists:get_value(public_keys, CryptoSupport)) of
	true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdh_rsa"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdh_rsa"]),
            ClientChain = proplists:get_value(client_chain, Config, default_cert_chain_conf()),
            ServerChain = proplists:get_value(server_chain, Config, default_cert_chain_conf()),
            CertChainConf = gen_conf(ecdh_rsa, ecdh_rsa, ClientChain, ServerChain),
            GenCertData = public_key:pkix_test_data(CertChainConf),
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),

	    [{server_ecdh_rsa_opts, [{ssl_imp, new},{reuseaddr, true} | ServerConf]},
				    
	     {server_ecdh_rsa_verify_opts, [{ssl_imp, new},{reuseaddr, true}, 	 
                                            {verify, verify_peer} | ServerConf]},
					
	     {client_ecdh_rsa_opts, ClientConf}
				   
             | Config];
	_ ->
	    Config
    end.

make_rsa_ecdsa_cert(Config, Curve) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(ecdh, proplists:get_value(public_keys, CryptoSupport)) of
	true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config),
                                            "rsa_ecdsa_" ++ atom_to_list(Curve)]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config),
                                            "rsa_ecdsa_" ++ atom_to_list(Curve)]),
            ClientChain = proplists:get_value(client_chain, Config, default_cert_chain_conf()),
            ServerChain = proplists:get_value(server_chain, Config, default_cert_chain_conf()),
            CertChainConf = gen_conf(ecdh_rsa, ecdh_rsa, ClientChain, ServerChain, Curve),
            GenCertData = public_key:pkix_test_data(CertChainConf),
            [{server_config, ServerConf},
             {client_config, ClientConf}] =
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),

	    [{server_rsa_ecdsa_opts, [{reuseaddr, true} | ServerConf]},
	     {server_rsa_ecdsa_verify_opts, [{ssl_imp, new},{reuseaddr, true},
                                            {verify, verify_peer} | ServerConf]},
	     {client_rsa_ecdsa_opts, [{verify, verify_none} | ClientConf]} | Config];
	_ ->
	    Config
    end.


start_upgrade_server(Args) ->
    Node = proplists:get_value(node, Args),
    Result = spawn_link(Node, ?MODULE, run_upgrade_server, [Args]),
    receive
	{listen, up} ->
	    Result
    end.

run_upgrade_server(Opts) ->
    Port = proplists:get_value(port, Opts),
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    TcpOptions = proplists:get_value(tcp_options, Opts),
    SslOptions = proplists:get_value(ssl_options, Opts),
    Pid = proplists:get_value(from, Opts),

    ?CT_LOG("~ngen_tcp:listen(~p, ~p)~n", [Port, TcpOptions]),
    {ok, ListenSocket} = gen_tcp:listen(Port, TcpOptions),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    ?CT_LOG("~ngen_tcp:accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),

    try
	{ok, SslAcceptSocket} = case TimeOut of
				    infinity ->
					?CT_LOG("~nssl:handshake(~p, ~p)~n",
							   [AcceptSocket, SslOptions]),
					ssl:handshake(AcceptSocket, SslOptions);
				    _ ->
					?CT_LOG("~nssl:handshake(~p, ~p, ~p)~n",
							   [AcceptSocket, SslOptions, TimeOut]),
					ssl:handshake(AcceptSocket, SslOptions, TimeOut)
				end,
	{Module, Function, Args} = proplists:get_value(mfa, Opts),
	Msg = apply(Module, Function, [SslAcceptSocket | Args]),
	?CT_LOG("~nUpgrade Server Msg: ~p ~n", [Msg]),
	Pid ! {self(), Msg},
	receive
	    close ->
		?CT_LOG("~nUpgrade Server closing~n", []),
		ssl:close(SslAcceptSocket)
	end
    catch error:{badmatch, Error} ->
	    Pid ! {self(), Error}
    end.

start_upgrade_client(Args) ->
    Node = proplists:get_value(node, Args),
    spawn_link(Node, ?MODULE, run_upgrade_client, [Args]).

run_upgrade_client(Opts) ->
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    TcpOptions = proplists:get_value(tcp_options, Opts),
    SslOptions = proplists:get_value(ssl_options, Opts),
    
    ?CT_LOG("~ngen_tcp:connect(~p, ~p, ~p)~n",
		       [Host, Port, TcpOptions]),
    {ok, Socket} = gen_tcp:connect(Host, Port, TcpOptions),

    send_selected_port(Pid, Port, Socket),

    ?CT_LOG("~nssl:connect(~p, ~p)~n", [Socket, SslOptions]),
    {ok, SslSocket} = ssl:connect(Socket, SslOptions),

    {Module, Function, Args} = proplists:get_value(mfa, Opts),
    ?CT_LOG("~napply(~p, ~p, ~p)~n",
		       [Module, Function, [SslSocket | Args]]),
    Msg = apply(Module, Function, [SslSocket | Args]),
    ?CT_LOG("~nUpgrade Client Msg: ~p ~n", [Msg]),
    Pid ! {self(), Msg},
    receive 
	close ->
	    ?CT_LOG("~nUpgrade Client closing~n", []),
	    ssl:close(SslSocket)
    end.

start_upgrade_client_error(Args) ->
    Node = proplists:get_value(node, Args),
    spawn_link(Node, ?MODULE, run_upgrade_client_error, [Args]).

run_upgrade_client_error(Opts) ->
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    Timeout = proplists:get_value(timeout, Opts, infinity),
    TcpOptions = proplists:get_value(tcp_options, Opts),
    SslOptions = proplists:get_value(ssl_options, Opts),
    ?CT_LOG("gen_tcp:connect(~p, ~p, ~p)",
               [Host, Port, TcpOptions]),
    {ok, Socket} = gen_tcp:connect(Host, Port, TcpOptions),
    send_selected_port(Pid, Port, Socket),
    ?CT_LOG("ssl:connect(~p, ~p)", [Socket, SslOptions]),
    Error = ssl:connect(Socket, SslOptions, Timeout),
    Pid ! {self(), Error}.

start_upgrade_server_error(Args) ->
    Node = proplists:get_value(node, Args),
    Result = spawn_link(Node,?MODULE, run_upgrade_server_error, [Args]),
    receive
	{listen, up} ->
	    Result
    end.

run_upgrade_server_error(Opts) ->
    Port = proplists:get_value(port, Opts),
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    TcpOptions = proplists:get_value(tcp_options, Opts),
    SslOptions = proplists:get_value(ssl_options, Opts),
    Pid = proplists:get_value(from, Opts),

    ?CT_LOG("~ngen_tcp:listen(~p, ~p)~n", [Port, TcpOptions]),
    {ok, ListenSocket} = gen_tcp:listen(Port, TcpOptions),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    ?CT_LOG("~ngen_tcp:accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    Error = case TimeOut of
		infinity ->
		    ?CT_LOG("~nssl:handshake(~p, ~p)~n",
				       [AcceptSocket, SslOptions]),
		    ssl:handshake(AcceptSocket, SslOptions);
		_ ->
		    ?CT_LOG("~nssl:ssl_handshake(~p, ~p, ~p)~n",
				       [AcceptSocket, SslOptions, TimeOut]),
		    ssl:handshake(AcceptSocket, SslOptions, TimeOut)
	    end,
    Pid ! {self(), Error}.

start_server_error(Args) ->
    Result = spawn_link(?MODULE, run_server_error, [Args]),
    receive 
	{listen, up} ->
	    Result
    end.

run_server_error(Opts) ->
    Port = proplists:get_value(port, Opts),
    Options = proplists:get_value(options, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    ?CT_LOG("~nssl:listen(~p, ~p)~n", [Port, Options]),
    Timeout = proplists:get_value(timeout, Opts, infinity),
    case Transport:listen(Port, Options) of
	{ok, #sslsocket{} = ListenSocket} ->
	    %% To make sure error_client will
	    %% get {error, closed} and not {error, connection_refused}
	    Pid ! {listen, up},
	    send_selected_port(Pid, Port, ListenSocket),
	    ?CT_LOG("~nssl:transport_accept(~p)~n", [ListenSocket]),
	    case Transport:transport_accept(ListenSocket, Timeout) of
		{error, _} = Error ->
		    Pid ! {self(), Error};
		{ok, AcceptSocket} ->
		    ?CT_LOG("~nssl:handshake(~p)~n", [AcceptSocket]),
		    Error = ssl:handshake(AcceptSocket),
		    Pid ! {self(), Error}
	    end;
	{ok, ListenSocket} ->
	    Pid ! {listen, up},
	    send_selected_port(Pid, Port, ListenSocket),
	    ?CT_LOG("~n~p:accept(~p)~n", [Transport, ListenSocket]),
	     case Transport:accept(ListenSocket) of
		{error, _} = Error ->
		     Pid ! {self(), Error}
	     end;
	Error ->
	    %% Not really true but as this is an error test 
	    %% this is what we want.
	    Pid ! {listen, up},
	    Pid ! {self(), Error}
    end.

start_client_error(Args) ->
    Node = proplists:get_value(node, Args),
    spawn_link(Node, ?MODULE, run_client_error, [Args]).

run_client_error(Opts) ->
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport = proplists:get_value(transport, Opts, ssl),
    Options0 = proplists:get_value(options, Opts),
    Options = patch_dtls_options(Options0),
    ?CT_LOG("~nssl:connect(~p, ~p, ~p)~n", [Host, Port, Options]),
    Error = Transport:connect(Host, Port, Options),
    case Error of
        {error, _} ->
            Pid ! {self(), Error};
        {ok, _Socket} ->
            receive
                {ssl_error, _, {tls_alert, _}} = SslError ->
                                Pid ! {self(), SslError}
            end;
        {ok, Socket, _Ext} ->
            ContOpts = proplists:get_value(continue_options, Opts, []),
            Result = Transport:handshake_continue(Socket, ContOpts),
            Pid ! {self(), Result}
    end.

accepters(N) ->
    accepters([], N).

accepters(Acc, 0) ->
    Acc;
accepters(Acc, N) ->
    receive 
	{accepter, _, Server} ->
	    accepters([Server| Acc], N-1)
    end.


basic_test(COpts, SOpts, Config) ->
    SType = proplists:get_value(server_type, Config, erlang),
    CType = proplists:get_value(client_type, Config, erlang),
    {Server, Port} = start_server(SType,  COpts, ssl_options(SOpts, Config), Config),
    Client = start_client(CType, Port, ssl_options(COpts, Config), Config),
    gen_check_result(Server, SType, Client, CType),
    stop(Server, Client).    

basic_alert(ClientOpts, ServerOpts, Config, Alert) ->
    SType = proplists:get_value(server_type, Config),
    CType = proplists:get_value(client_type, Config),
    run_basic_alert(SType, CType, ClientOpts, ServerOpts, Config, Alert).

run_basic_alert(erlang, erlang, ClientOpts, ServerOpts, Config, Alert) ->
    {ClientNode, ServerNode, Hostname} = run_where(Config),
    
    Server = start_server_error([{node, ServerNode}, {port, 0},
                                 {from, self()},
                                 {mfa, {ssl_test_lib, no_result, []}},
                                 {options, ServerOpts}]),
    
    Port  = inet_port(Server),
    
    Client = start_client_error([{node, ClientNode}, {port, Port},
                                 {host, Hostname},
                                 {from, self()},
                                 {mfa, {ssl_test_lib, no_result, []}},
                                 {options, ClientOpts}]),

    check_server_alert(Server, Client, Alert);
run_basic_alert(openssl = SType, erlang, ClientOpts, ServerOpts, Config, Alert) ->
    {ClientNode, _, Hostname} = run_where(Config),
    {_Server, Port} = start_server(SType, ClientOpts, ServerOpts, Config),
    wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),
    Client = start_client_error([{node, ClientNode}, {port, Port},
                                 {host, Hostname},
                                 {from, self()},
                                 {mfa, {ssl_test_lib, no_result, []}},
					      {options, ClientOpts}]),
    
    check_client_alert(Client, Alert);
run_basic_alert(erlang, openssl = CType, ClientOpts, ServerOpts, Config, Alert) ->
    {_, ServerNode, Hostname} = run_where(Config),
    Server = start_server_error([{node, ServerNode}, {port, 0},
                                 {host, Hostname},
                                 {from, self()},
                                 {mfa, {ssl_test_lib, no_result, []}},
                                 {options, ServerOpts}]),
    Port  = inet_port(Server),
    start_client(CType, Port, ClientOpts, Config),

    check_server_alert(Server, Alert).
    

ecc_test(Expect, COpts, SOpts, CECCOpts, SECCOpts, Config) ->
    {Server, Port} = start_server_ecc(erlang, SOpts, Expect, SECCOpts, Config),
    Client = start_client_ecc(erlang, Port, COpts, Expect, CECCOpts, Config),
    check_result(Server, ok, Client, ok),
    stop(Server, Client).

ecc_test_error(COpts, SOpts, CECCOpts, SECCOpts, Config) ->
    {Server, Port} = start_server_ecc_error(erlang, SOpts, SECCOpts, Config),
    Client = start_client_ecc_error(erlang, Port, COpts, CECCOpts, Config),
    check_server_alert(Server, Client, insufficient_security).

start_client(openssl, Port, ClientOpts, Config) ->
    Version = protocol_version(Config),
    Exe = "openssl",
    DOpenssl = proplists:get_value(debug_openssl, ClientOpts, false),
    Ciphers = proplists:get_value(ciphers, ClientOpts, ssl:cipher_suites(default,Version)),
    Groups0 = proplists:get_value(groups, ClientOpts),
    CertArgs = openssl_cert_options(ClientOpts, client),
    SigAlgs = openssl_sigalgs(proplists:get_value(sigalgs, ClientOpts, undefined)),
    AlpnArgs = openssl_alpn_options(proplists:get_value(alpn, ClientOpts, undefined)),
    NpnArgs =  openssl_npn_options(proplists:get_value(np, ClientOpts, undefined)),                          
    Reconnect = openssl_reconect_option(proplists:get_value(reconnect, ClientOpts, false)),  
    MaxFragLen = openssl_maxfag_option(proplists:get_value(maxfrag, ClientOpts, false)),
    SessionArgs =  proplists:get_value(session_args, ClientOpts, []),
    HostName = proplists:get_value(hostname, ClientOpts, net_adm:localhost()),
    SNI = openssl_sni(proplists:get_value(server_name_indication, ClientOpts, undefined)),
    Debug = openssl_debug_options(DOpenssl),

    Exe = "openssl",
    Args0 =  case Groups0 of
                undefined ->
                     ["s_client",
                      "-verify", "2",
                      "-connect", hostname_format(HostName) ++ ":" ++ integer_to_list(Port),
                      cipher_flag(Version),
                      ciphers(Ciphers, Version),
                      version_flag(Version)] ++
                         CertArgs ++
                         SigAlgs ++
                         SNI ++
                         AlpnArgs ++
                         NpnArgs ++
                         Reconnect ++
                         MaxFragLen ++
                         SessionArgs ++
                         Debug;
                 Group ->
                     ["s_client",
                      "-verify", "2",
                      "-connect", hostname_format(HostName) ++ ":" ++ integer_to_list(Port),
                      cipher_flag(Version),
                      ciphers(Ciphers, Version),
                      "-groups", Group,
                      version_flag(Version)] ++
                         CertArgs ++
                         SigAlgs ++
                         SNI ++
                         AlpnArgs ++
                         NpnArgs ++
                         Reconnect ++
                         MaxFragLen ++
                         SessionArgs ++
                         Debug
             end,
    Args = maybe_force_ipv4(Args0),
    OpenSslPort = portable_open_port(Exe, Args),
    true = port_command(OpenSslPort, "Hello world"),
    OpenSslPort;

start_client(erlang, Port, ClientOpts, Config) ->
    {ClientNode, _, Hostname} = run_where(Config),
    KeyEx = proplists:get_value(check_keyex, Config, false),
    start_client([{node, ClientNode}, {port, Port},
			       {host, Hostname},
			       {from, self()},
			       {mfa, {ssl_test_lib, check_key_exchange_send_active, [KeyEx]}},
			       {options, ClientOpts}]).

%% Workaround for running tests on machines where openssl
%% s_client would use an IPv6 address with localhost. As
%% this test suite and the ssl application is not prepared
%% for that we have to force s_client to use IPv4 if
%% OpenSSL supports IPv6.
maybe_force_ipv4(Args0) ->
    case is_ipv6_supported() of
        true ->
            Args0 ++ ["-4"];
        false ->
            Args0
    end.

start_client_ecc(erlang, Port, ClientOpts, Expect, ECCOpts, Config) ->
    {ClientNode, _, Hostname} = run_where(Config),
    ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                               {host, Hostname},
                               {from, self()},
                               {mfa, {?MODULE, check_ecc, [client, Expect]}},
                               {options,
                                ECCOpts ++
                                [{verify, verify_peer} | ClientOpts]}]).

start_client_ecc_error(erlang, Port, ClientOpts, ECCOpts, Config) ->
    {ClientNode, _, Hostname} = run_where(Config),
    ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                     {host, Hostname},
                                     {from, self()},
                                     {options,
                                      ECCOpts ++
                                      [{verify, verify_peer} | ClientOpts]}]).


start_server(openssl, ClientOpts, ServerOpts, Config) ->
    Port = inet_port(node()),
    Version = protocol_version(Config),
    Exe = "openssl",
    CertArgs = openssl_cert_options(ServerOpts, server),
    Ciphers = proplists:get_value(ciphers, ClientOpts, ssl:cipher_suites(default,Version)),
    Groups0 = proplists:get_value(groups, ServerOpts),
    SigAlgs = proplists:get_value(openssl_sigalgs, Config, undefined),
    SessionArgs = proplists:get_value(session_args, Config, []),
    DOpenssl = proplists:get_value(debug_openssl, ServerOpts, false),
    Debug = openssl_debug_options(DOpenssl),

    Args =  case Groups0 of
                undefined ->
                    ["s_server", "-accept", integer_to_list(Port), cipher_flag(Version),
                     ciphers(Ciphers, Version),
                     version_flag(Version)] ++ sig_algs(SigAlgs) ++ CertArgs ++ SessionArgs ++ Debug;
                Group ->
                       ["s_server", "-accept", integer_to_list(Port), cipher_flag(Version),
                        ciphers(Ciphers, Version), "-groups", Group, 
                        version_flag(Version)] ++ sig_algs(SigAlgs) ++ CertArgs ++ SessionArgs ++ Debug
            end,
    OpenSslPort = portable_open_port(Exe, Args),
    true = port_command(OpenSslPort, "Hello world"),
    {OpenSslPort, Port};
start_server(erlang, _, ServerOpts, Config) ->
    {_, ServerNode, _} = run_where(Config),
    KeyEx = proplists:get_value(check_keyex, Config, false),
    Server = start_server([{node, ServerNode}, {port, 0},
                           {from, self()},
                           {mfa, {ssl_test_lib,
                                  check_key_exchange_send_active,
                                  [KeyEx]}},
                           {options, ServerOpts}]),
    {Server, inet_port(Server)}.
 
sig_algs(undefined) ->
    [];
sig_algs(SigAlgs) ->
    ["-sigalgs " ++ SigAlgs]. 

cipher_flag('tlsv1.3') ->
     "-ciphersuites";
cipher_flag(_) ->
    "-cipher".

ciphers([#{}| _] = Ciphers, Version) ->
    Strs = [ssl_cipher_format:suite_map_to_openssl_str(Cipher) || Cipher <- Ciphers],
    ciphers_concat(Version, Strs, "");
ciphers(Ciphers, Version) ->
    ciphers_concat(Version, Ciphers, "").

ciphers_concat(_, [], [":" | Acc]) ->
    lists:append(lists:reverse(Acc));
ciphers_concat('tlsv1.3' = Version, [Head| Tail], Acc) ->
    case Head of
        "TLS" ++ _ ->
            ciphers_concat(Version, Tail, [":", Head | Acc]);
        _ ->
            ciphers_concat(Version, Tail, Acc)
    end;
ciphers_concat(Version,  [Head| Tail], Acc) ->
    ciphers_concat(Version, Tail, [":", Head | Acc]).

openssl_alpn_options(undefined) ->
    [];
openssl_alpn_options(Alpn) ->
    ["-alpn", Alpn].

openssl_npn_options(undefined) ->
    [];
openssl_npn_options(Npn) ->
    ["-nextprotoneg", Npn].

openssl_reconect_option(false) ->
    [];
openssl_reconect_option(true) ->
    ["-reconnect"].
openssl_maxfag_option(false) ->
    [];
openssl_maxfag_option(Int) ->
    ["-maxfraglen", integer_to_list(Int)].

openssl_sni(undefined) ->
    [];
openssl_sni(disable) ->
    ["-noservername"];
openssl_sni(ServerName) ->
    ["-servername", ServerName].

openssl_debug_options(true) ->
    ["-msg", "-debug"];
openssl_debug_options(false) ->
    [].
%%
openssl_debug_options(PrivDir, true) ->
    case is_keylogfile_supported() of
        true ->
            ["-msg", "-debug","-keylogfile", PrivDir ++ "keylog"];
        false ->
            ["-msg", "-debug"]
    end;
openssl_debug_options(_, false) ->
    [].

is_keylogfile_supported() ->
    [{_,_, Bin}]  = crypto:info_lib(),
    case binary_to_list(Bin) of
	"OpenSSL 1.1.1" ++ _ ->
	    true;
	_ ->
	    false
    end.

start_server_with_raw_key(erlang, ServerOpts, Config) ->
    {_, ServerNode, _} = run_where(Config),
    Server = start_server([{node, ServerNode}, {port, 0},
                           {from, self()},
                           {mfa, {ssl_test_lib,
                                  send_recv_result_active,
                                  []}},
                           {options,
                            [{verify, verify_peer} | ServerOpts]}]),
    {Server, inet_port(Server)}.

start_server_ecc(erlang, ServerOpts, Expect, ECCOpts, Config) ->
    {_, ServerNode, _} = run_where(Config),
    Server = start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {?MODULE, check_ecc, [server, Expect]}},
                                        {options,
                                         ECCOpts ++
                                         [{verify, verify_peer} | ServerOpts]}]),
    {Server, inet_port(Server)}.

start_server_ecc_error(erlang, ServerOpts, ECCOpts, Config) ->
    {_, ServerNode, _} = run_where(Config),
    Server = start_server_error([{node, ServerNode}, {port, 0},
                                              {from, self()},
                                              {options,
                                               ECCOpts ++
                                               [{verify, verify_peer} | ServerOpts]}]),
    {Server, inet_port(Server)}.

gen_check_result(Server, erlang, Client, erlang) ->
    check_result(Server, ok, Client, ok);
gen_check_result(Server, erlang, _, _) ->
    check_result(Server, ok);
gen_check_result(_, _, Client, erlang) ->
    check_result(Client, ok);
gen_check_result(_,openssl, _, openssl) ->
    ok.

stop(Port1, Port2) when is_port(Port1), is_port(Port2) ->
    close_port(Port1),
    close_port(Port2);
stop(Port, Pid) when is_port(Port) ->
    close_port(Port),
    close(Pid);
stop(Pid, Port) when is_port(Port) ->
    close_port(Port),
    close(Pid);
stop(Client, Server)  ->
    close(Server),
    close(Client).


openssl_cert_options(Opts, Role) ->
    Cert = proplists:get_value(certfile, Opts, undefined),
    Key = proplists:get_value(keyfile, Opts, undefined),
    CA = proplists:get_value(cacertfile, Opts, undefined),
    case CA of
        undefined ->
            case cert_option("-cert", Cert) ++ cert_option("-key", Key) of
                [] when Role == server ->
                    ["-nocert"];
                Other ->
                    Other
            end;
        _ ->
            cert_option("-cert", Cert) ++  cert_option("-CAfile", CA)
                ++ cert_option("-cert_chain", CA) ++ 
                cert_option("-key", Key) ++ openssl_verify(Opts) ++ ["2"]
    end.

openssl_verify(Opts) ->
      case proplists:get_value(fail_if_no_peer_cert, Opts, undefined) of
          true ->
              ["-Verify"];
          _ ->
              ["-verify"]
      end.
    
cert_option(_, undefined) ->
    [];
cert_option("-cert_chain", Value) ->
    case portable_cmd("openssl", ["version"]) of
	"OpenSSL 1.1.1" ++ _ ->
	    ["-cert_chain", Value];
        _ ->
            ""
     end;
cert_option(Opt, Value) ->
    [Opt, Value].

openssl_sigalgs(undefined) ->
    [];
openssl_sigalgs(SigAlgs) ->
    ["-sigalgs", SigAlgs].

supported_eccs(Opts) ->
    ToCheck = proplists:get_value(eccs, Opts, []),
    Supported = ssl:eccs(),
    lists:all(fun(Curve) -> lists:member(Curve, Supported) end, ToCheck).

check_ecc(SSL, Role, Expect) ->
    {ok, Data} = ssl:connection_information(SSL),
    case lists:keyfind(ecc, 1, Data) of
        {ecc, {named_curve, Expect}} -> ok;
        Other -> {error, Role, Expect, Other}
    end.

inet_port(Pid) when is_pid(Pid)->
    receive
	{Pid, {port, Port}} ->
	    Port
    end;

inet_port(Node) ->
    {Port, Socket} = do_inet_port(Node),
     rpc:call(Node, gen_tcp, close, [Socket]),
     Port.

do_inet_port(Node) ->
    {ok, Socket} = rpc:call(Node, gen_tcp, listen, [0, [{reuseaddr, true}]]),
    {ok, Port} = rpc:call(Node, inet, port, [Socket]),
    {Port, Socket}.

no_result(_) ->
    no_result_msg.

trigger_renegotiate(Socket, [ErlData, N]) ->
     {ok, [{session_id, Id}]} = ssl:connection_information(Socket,  [session_id]),
    trigger_renegotiate(Socket, ErlData, N, Id).

trigger_renegotiate(Socket, _, 0, Id) ->
    ct:sleep(1000),
    case ssl:connection_information(Socket,  [session_id]) of
        {ok, [{session_id, Id}]} ->
	    fail_session_not_renegotiated;
	%% Tests that uses this function will not reuse
	%% sessions so if we get a new session id the
	%% renegotiation has succeeded.
        {ok, [{session_id, _}]} -> 
	    ok;
	{error, closed} ->
	    fail_session_fatal_alert_during_renegotiation;
	{error, timeout} ->
	    fail_timeout
    end;

trigger_renegotiate(Socket, ErlData, N, Id) ->
    ssl:send(Socket, ErlData),
    trigger_renegotiate(Socket, ErlData, N-1, Id).				   
    

send_selected_port(Pid, 0, #sslsocket{} = Socket) ->
    {ok, {_, NewPort}} = ssl:sockname(Socket),	 
    Pid ! {self(), {port, NewPort}};
send_selected_port(Pid, 0, Socket) ->
    {ok, {_, NewPort}} = inet:sockname(Socket),	 
    Pid ! {self(), {port, NewPort}};
send_selected_port(_,_,_) ->
    ok.


available_suites(Version) ->
    [ssl_cipher_format:suite_bin_to_map(Suite) || 
	Suite  <-  ssl_cipher:filter_suites(ssl_cipher:suites(Version))].


rsa_non_signed_suites(Version) ->
    lists:filter(fun({rsa, _, _}) ->
			 false;
		    (_) ->
			 true
		 end,
		 available_suites(Version)).
dsa_suites(Version) ->
    ssl:filter_cipher_suites(available_suites(Version),
                             [{key_exchange,
                               fun(dhe_dss) ->
                                       true;
                                  (_) ->
                                       false
                               end}]).

openssl_dsa_suites() ->
    Ciphers = openssl_ciphers(),
    lists:filter(fun(Str) -> string_regex_filter(Str, "DSS")
		 end, Ciphers).

openssl_ecdsa_suites() ->
    Ciphers = openssl_ciphers(),
    lists:filter(fun(Str) -> string_regex_filter(Str, "ECDHE-ECDSA")
		 end, Ciphers).

string_regex_filter(Str, Search) when is_list(Str) ->
    case re:run(Str, Search, []) of
	nomatch ->
	    false;
	_ ->
	    true
    end;
string_regex_filter(_Str, _Search) ->
    false.

ecdh_dh_anonymous_suites(Version) ->
    ssl:filter_cipher_suites([ssl_cipher_format:suite_bin_to_map(S) || S <- ssl_cipher:anonymous_suites(Version)],
                             [{key_exchange, 
                               fun(dh_anon) -> 
                                       true;
                                  (ecdh_anon) -> 
                                       true;
                                  (_) -> 
                                       false 
                               end}]).

pem_to_der(File) ->
    {ok, PemBin} = file:read_file(File),
    public_key:pem_decode(PemBin).

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).

cipher_result(Socket, Result) ->
    {ok, Info} = ssl:connection_information(Socket),
    Result = {ok, {proplists:get_value(protocol, Info), proplists:get_value(selected_cipher_suite, Info)}},
    ?CT_LOG("~nSuccessfull connect: ~p~n", [Result]),
    %% Importante to send two packets here
    %% to properly test "cipher state" handling
    Hello = "Hello\n",
    World = " world\n",
    ssl:send(Socket, Hello),
    ct:sleep(500),
    ssl:send(Socket, World),
    Expected = Hello ++ World,
    Expected = active_recv(Socket, length(Expected)),
    ok.

session_info_result(Socket) ->
    {ok, Info} = ssl:connection_information(Socket,  [session_id, cipher_suite]),
    Info.

public_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?rsaEncryption},
			     privateKey = Key}) ->
    public_key:der_decode('RSAPrivateKey', iolist_to_binary(Key));

public_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-dsa'},
			     privateKey = Key}) ->
    public_key:der_decode('DSAPrivateKey', iolist_to_binary(Key));
public_key(Key) ->
    Key.

state([{data,[{"State", {_StateName, StateData}}]} | _]) -> %% gen_statem
    StateData;
state([{data,[{"State", State}]} | _]) -> %% gen_server
    State;
state([{data,[{"StateData", State}]} | _]) -> %% gen_fsm
     State;
state([_ | Rest]) ->
    state(Rest).

is_protocol_version(Ver) ->
    is_tls_version(Ver) orelse
        is_dtls_version(Ver).

is_tls_version('tlsv1.3') ->
    true;
is_tls_version('tlsv1.2') ->
    true;
is_tls_version('tlsv1.1') ->
    true;
is_tls_version('tlsv1') ->
    true;
is_tls_version(_) ->
    false.

is_dtls_version('dtlsv1.2') ->
    true;
is_dtls_version('dtlsv1') ->
    true;
is_dtls_version(_) ->
    false.

openssl_tls_version_support(Version, Config0) ->
    Config = make_rsa_cert(Config0),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    Port = inet_port(node()),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Exe = "openssl",
    Opts0 = [{versions, [Version]}, {verify, verify_none}],
    TLSOpts = [{protocol,tls}|Opts0],
    DTLSOpts = patch_dtls_options([{protocol, dtls}|Opts0]),

    TLSArgs = ["s_server", "-accept",
               integer_to_list(Port), "-CAfile", CaCertFile,
               "-cert", CertFile,"-key", KeyFile],
    DTLSArgs = ["s_server", "-accept",
                integer_to_list(Port), "-dtls", "-CAfile", CaCertFile,
                "-cert", CertFile,"-key", KeyFile],

    case is_tls_version(Version) of
        true ->
            openssl_tls_version_support(tls, TLSOpts, Port, Exe, TLSArgs);
        false ->
            DTLSTupleVersion = dtls_record:protocol_version_name(Version),
            CorrespondingTLSVersion = dtls_v1:corresponding_tls_version(DTLSTupleVersion),
            AtomTLSVersion = tls_record:protocol_version(CorrespondingTLSVersion),
            CorrTLSOpts = [{protocol,tls}, {versions, [AtomTLSVersion]},
                           {verify, verify_none}],
            case openssl_tls_version_support(tls, CorrTLSOpts, Port, Exe, TLSArgs) of
                true ->
                    %% If corresponding TLS version is not supported DTLS
                    %% will not be supported and test for it will be inconclusive
                    %% due to UDP not being a reliable transport
                    openssl_tls_version_support(dtls, DTLSOpts, Port, Exe, DTLSArgs);
                false ->
                    false
            end
    end.

openssl_tls_version_support(Proto, Opts, Port, Exe, Args0) ->
    Args = maybe_force_ipv4(Args0),
    OpensslPort = portable_open_port(Exe, Args),
    try wait_for_openssl_server(Port, Proto) of
        ok ->
            case  ssl:connect("localhost", Port, Opts, 5000) of
                {ok, Socket} ->
                    ssl:close(Socket),
                    close_port(OpensslPort),
                    true;
                {error, {tls_alert, {protocol_version, _}}} ->
                    ?CT_LOG("OpenSSL does not support ~p", [proplists:get_value(versions, Opts)]),
                    close_port(OpensslPort),
                    false;
                {error, {tls_alert, Alert}} ->
                    ?CT_LOG("OpenSSL returned alert ~p", [Alert]),
                    close_port(OpensslPort),
                    false;
                {error, timeout} ->
                    ?CT_LOG("Timed out connection to OpenSSL", []),
                    close_port(OpensslPort),
                    false
            end
    catch
        _:_ ->
            ?CT_LOG("OpenSSL does not support ~p", [proplists:get_value(versions, Opts)]),
            close_port(OpensslPort),
            false
    end.

init_protocol_version(Version, Config)
  when Version == 'dtlsv1.2'; Version == 'dtlsv1' ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, dtls_protocol_version, [Version]),
    ssl:start(),
    NewConfig = proplists:delete(protocol_opts, proplists:delete(protocol, Config)),
    [{protocol, dtls}, {protocol_opts, [{protocol, dtls}]} | NewConfig];

init_protocol_version(Version, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, protocol_version, [Version]),
    ssl:start(),
    NewConfig = proplists:delete(protocol_opts, proplists:delete(protocol, Config)),
    [{protocol, tls} | NewConfig].

clean_protocol_version(Config) ->
    application:unset_env(ssl, protocol_version),
    application:unset_env(ssl, dtls_protocol_version),
    proplists:delete(version, proplists:delete(protocol_opts, proplists:delete(protocol, Config))).

sufficient_crypto_support(Version)
  when Version == 'tlsv1.3' ->
    CryptoSupport = crypto:supports(),
    lists:member(rsa_pkcs1_pss_padding, proplists:get_value(rsa_opts, CryptoSupport)) andalso
    lists:member(x448, proplists:get_value(curves, CryptoSupport));
sufficient_crypto_support(Version)
  when Version == 'tlsv1.2'; Version == 'dtlsv1.2' ->
    CryptoSupport = crypto:supports(),
    proplists:get_bool(sha256, proplists:get_value(hashs, CryptoSupport));
sufficient_crypto_support(cipher_ec) -> 
    CryptoSupport = crypto:supports(),
    proplists:get_bool(ecdh, proplists:get_value(public_keys, CryptoSupport));
sufficient_crypto_support(_) ->
    true.

check_key_exchange_send_active(Socket, false) ->
    send_recv_result_active(Socket);
check_key_exchange_send_active(Socket, KeyEx) ->
    {ok, Info} =
        ssl:connection_information(Socket, [cipher_suite, protocol]),
    Suite = proplists:get_value(cipher_suite, Info),
    Version = proplists:get_value(protocol, Info),
    true = check_key_exchange(Suite, KeyEx, Version), 
    send_recv_result_active(Socket).

check_key_exchange({KeyEx,_, _}, KeyEx, _) ->
    ?CT_LOG("Kex: ~p", [KeyEx]),
    true;
check_key_exchange({KeyEx,_,_,_}, KeyEx, _) ->
    ?CT_LOG("Kex: ~p", [KeyEx]),
    true;
check_key_exchange(KeyEx1, KeyEx2, Version) ->
    ?CT_LOG("Kex: ~p ~p", [KeyEx1, KeyEx2]),
    case Version of
        'tlsv1.2' ->
            v_1_2_check(element(1, KeyEx1), KeyEx2);
        'dtlsv1.2' ->
            v_1_2_check(element(1, KeyEx1), KeyEx2);
        _ ->       
            ?CT_LOG("Negotiated ~p  Expected ~p", [KeyEx1, KeyEx2]),
            false
    end.

v_1_2_check(ecdh_ecdsa, ecdh_rsa) ->
    true;
v_1_2_check(ecdh_rsa, ecdh_ecdsa) ->
    true;
v_1_2_check(_, _) ->
    false.

send_recv_result(Socket) ->
    Data =  "Hello world",
    ssl:send(Socket, Data),
    {ok, Data} = ssl:recv(Socket, length(Data)),
    ok.

send_recv_result_active(Socket) ->
    Data =  "Hello world",
    ssl:send(Socket, Data),
    Data = active_recv(Socket, length(Data)),
    ok.

send_recv_result_active(Socket, Data) ->
    ssl:send(Socket, Data),
    Data = active_recv(Socket, length(Data)),
    ok.

send(Pid, Data) ->
    Pid ! {data, Data},
    receive
        {Pid, ok} ->
            ok;
        {Pid, Reason} ->
            {error, Reason}
    end.

check_active_receive(Pid, Data) ->
    Pid ! {active_receive, Data},
    check_active_receive_loop(Pid, Data).

check_active_receive_loop(Pid, Data) ->
    receive
        {Pid, Data} ->
            ?CT_LOG("Received: ~p~n   (from ~p)~n", [Data, Pid]),
            Data;
        {Pid, Data2} ->
            ?CT_LOG("Received unexpected message: ~p~n   (from ~p)~n", [Data2, Pid]),
            check_active_receive_loop(Pid, Data)
    end.

update_keys(Pid, Type) ->
    Pid ! {update_keys, Type},
    receive
        {Pid, ok} ->
            ok;
        {Pid, Reason} ->
            {error, Reason}
    end.

send_recv_result_active_once(Socket) ->
    Data = "Hello world",
    ssl:send(Socket, Data),
    active_once_recv_list(Socket, length(Data)).

%% This function can verify the following functionalities in clients:
%% - session resumption, sending/receiving application data, receiving session tickets
%% - verifying if client early data is accepted/rejected
verify_active_session_resumption(Socket, SessionResumption) ->
    verify_active_session_resumption(Socket, SessionResumption, wait_reply, no_tickets, no_early_data).
%%
verify_active_session_resumption(Socket, SessionResumption, WaitReply) ->
    verify_active_session_resumption(Socket, SessionResumption, WaitReply, no_tickets, no_early_data).
%%
verify_active_session_resumption(Socket, SessionResumption, WaitReply, TicketOption) ->
    verify_active_session_resumption(Socket, SessionResumption, WaitReply, TicketOption, no_early_data).
%%
verify_active_session_resumption(Socket, SessionResumption, WaitForReply, TicketOption, EarlyData) ->
    case ssl:connection_information(Socket, [session_resumption]) of
        {ok, [{session_resumption, SessionResumption}]} ->
            Msg = boolean_to_log_msg(SessionResumption),
            ?CT_LOG("~nSession resumption verified! (expected ~p, got ~p)!",
                   [Msg, Msg]);
        {ok, [{session_resumption, Got0}]} ->
            Expected = boolean_to_log_msg(SessionResumption),
            Got = boolean_to_log_msg(Got0),
            ?CT_FAIL("~nFailed to verify session resumption! (expected ~p, got ~p)",
                    [Expected, Got]);
        {error, Reason} ->
            ?CT_FAIL("~nFailed to verify session resumption! Reason: ~p",
                    [Reason])
    end,

    Data =  "Hello world",
    ssl:send(Socket, Data),
    case WaitForReply of
        wait_reply ->
            Data = active_recv(Socket, length(Data));
        no_reply ->
            ok;
        Else1 ->
            ?CT_FAIL("~nFaulty parameter: ~p", [Else1])
    end,
    Tickets =
        case TicketOption of
            {tickets, N} ->
                receive_tickets(N);
            no_tickets ->
                ok;
            Else2 ->
                ?CT_FAIL("~nFaulty parameter: ~p", [Else2])
        end,
    case EarlyData of
        {verify_early_data, Atom} ->
            case verify_early_data(Atom) of
                ok ->
                    Tickets;
                Else ->
                    ?CT_FAIL("~nFailed to verify early_data! (expected ~p, got ~p)",
                            [Atom, Else])
            end;
        no_early_data ->
            Tickets;
        Else3 ->
            ?CT_FAIL("~nFaulty parameter: ~p", [Else3])
    end.

verify_server_early_data(Socket, WaitForReply, EarlyData) ->
    case ssl:connection_information(Socket, [session_resumption]) of
        {ok, [{session_resumption, true}]} ->
            Msg = boolean_to_log_msg(true),
            ?CT_LOG("~nSession resumption verified! (expected ~p, got ~p)!",
                   [Msg, Msg]);
        {ok, [{session_resumption, Got0}]} ->
            Expected = boolean_to_log_msg(true),
            Got = boolean_to_log_msg(Got0),
            ?CT_FAIL("~nFailed to verify session resumption! (expected ~p, got ~p)",
                    [Expected, Got]);
        {error, Reason} ->
            ?CT_FAIL("~nFailed to verify session resumption! Reason: ~p",
                    [Reason])
    end,
    Data =  "Hello world",
    ssl:send(Socket, Data),
    Reply =
        case EarlyData of
            no_early_data ->
                Data;
            _ ->
                binary_to_list(EarlyData) ++ Data
        end,
    ?CT_LOG("Expected Reply: ~p~n", [Reply]),
    case WaitForReply of
        wait_reply ->
            Reply = active_recv(Socket, length(Reply));
        no_reply ->
            ok;
        Else1 ->
            ?CT_FAIL("~nFaulty parameter: ~p", [Else1])
    end,
    ok.

verify_session_ticket_extension([Ticket0|_], MaxEarlyDataSize) ->
    #{ticket := #new_session_ticket{
                   extensions = #{early_data :=
                                      #early_data_indication_nst{
                                         indication = Size}}}} = Ticket0,
      case Size of
          MaxEarlyDataSize ->
              ?CT_LOG("~nmax_early_data_size verified! (expected ~p, got ~p)!",
                     [MaxEarlyDataSize, Size]);
          Else ->
              ?CT_LOG("~nFailed to verify max_early_data_size! (expected ~p, got ~p)!",
                     [MaxEarlyDataSize, Else])
      end.

update_session_ticket_extension([Ticket|_], MaxEarlyDataSize) ->
    #{ticket := #new_session_ticket{
                   extensions = #{early_data :=
                                      #early_data_indication_nst{
                                         indication = Size}}}} = Ticket,
    ?CT_LOG("~nOverwrite max_early_data_size (from ~p to ~p)!",
                     [Size, MaxEarlyDataSize]),
    #{ticket := #new_session_ticket{
                   extensions = #{early_data := _Extensions0}} = NST0} = Ticket,
    Extensions = #{early_data => #early_data_indication_nst{
                                    indication = MaxEarlyDataSize}},
    NST = NST0#new_session_ticket{extensions = Extensions},
    [Ticket#{ticket => NST}].

boolean_to_log_msg(true) ->
    "OK";
boolean_to_log_msg(false) ->
    "FAIL".

receive_tickets(N) ->
    receive_tickets(N, []).
%%
receive_tickets(0, Acc) ->
    Acc;
receive_tickets(N, Acc) ->
    receive
        {ssl, session_ticket, Ticket} ->
            receive_tickets(N - 1, [Ticket|Acc])
    end.

check_tickets(Client) ->
    receive
        {Client, Tickets} ->
            Tickets
    after
        5000 ->
            ?CT_FAIL("~nNo tickets received!", [])
    end.

active_recv_loop(Pid, SslPort, Data) ->
    case active_recv(SslPort, length(Data)) of
        Data ->
            ?CT_LOG("[openssl server] Received: ~p~n   (forward to PID=~p)~n",
                   [Data, Pid]),
            Pid ! {self(), Data};
        Unexpected ->
            ?CT_LOG("[openssl server] Received unexpected: ~p~n   (dropping message)~n",
                 [Unexpected]),
            active_recv_loop(Pid, SslPort, Data)
    end.

active_recv(Socket, N) ->
    active_recv(Socket, N, []).

active_recv(_Socket, 0, Acc) ->
    Acc;
active_recv(_Socket, N, Acc) when N < 0 ->
    {_, T} = lists:split(0 - N, Acc),
    T;
active_recv(Socket, N, Acc) ->
    receive 
        %% Filter {ssl, Socket, {early_data, Atom}} messages
	{ssl, Socket, Bytes} when not is_tuple(Bytes) ->
            active_recv(Socket, N-data_length(Bytes),  Acc ++ Bytes);
        {Socket, {data, Bytes0}} ->
            Bytes = filter_openssl_debug_data(Bytes0),
            active_recv(Socket, N-data_length(Bytes),  Acc ++ Bytes)
    end.


data_length(Bytes) when is_list(Bytes) ->
    length(Bytes);
data_length(Bytes) when is_binary(Bytes)->
    byte_size(Bytes).

filter_openssl_debug_data(Bytes) ->
    re:replace(Bytes,
               "(read.*\n|write to.*\n|[\\dabcdefABCDEF]{4,4} -.*\n|>>> .*\n|<<< .*\n|    \\d\\d.*\n|KEYUPDATE\n|.*Read BLOCK\n)*",
               "", [global,{return, list}]).

active_once_recv(_Socket, 0) ->
    ok;
active_once_recv(Socket, N) ->
    receive 
	{ssl, Socket, Bytes} ->
            ssl:setopts(Socket, [{active, once}]),
            active_once_recv(Socket, N-byte_size(Bytes))
    end.

active_once_recv_list(_Socket, 0) ->
    ok;
active_once_recv_list(Socket, N) ->
    receive 
	{ssl, Socket, Bytes} ->
            ssl:setopts(Socket, [{active, once}]),
            active_once_recv_list(Socket, N-length(Bytes))
    end.
recv_disregard(_Socket, 0) ->
    ok;
recv_disregard(Socket, N) ->
    {ok, Bytes} = ssl:recv(Socket, 0),
    recv_disregard(Socket, N-byte_size(Bytes)).

active_disregard(_Socket, 0) ->
    ok;
active_disregard(Socket, N) ->
    receive 
	{ssl, Socket, Bytes} ->
            active_disregard(Socket, N-byte_size(Bytes))
    end.
active_once_disregard(_Socket, 0) ->
    ok;
active_once_disregard(Socket, N) ->
    receive 
	{ssl, Socket, Bytes} ->
            ssl:setopts(Socket, [{active, once}]),
            active_once_disregard(Socket, N-byte_size(Bytes))
    end.

is_ipv6_supported() ->
    case portable_cmd("openssl", ["version"]) of
        "OpenSSL 0.9.8" ++ _ -> % Does not support IPv6
            false;
        "OpenSSL 1.0" ++ _ ->   % Does not support IPv6
            false;
        "LibreSSL 3" ++ _->
            false;
        _ ->
            true
    end.


is_sane_oppenssl_client() ->
    [{_,_, Bin}]  = crypto:info_lib(), 
    case binary_to_list(Bin) of
	"OpenSSL 0.9" ++ _ -> 
	    false;
	_ ->
	    true
    end.

is_sane_oppenssl_pss(rsa_pss_pss) ->
    case portable_cmd("openssl",["version"]) of
        "OpenSSL 3" ++ _ ->
            true;
        "OpenSSL 1.1.1" ++ Rest ->
            hd(Rest) >= $c;
        _ ->
            false
    end;
is_sane_oppenssl_pss(rsa_pss_rsae) ->
    case portable_cmd("openssl",["version"]) of
        "OpenSSL 3" ++ _ ->
            true;
        "OpenSSL 1.1.1" ++ _ ->
            true;
        _ ->
            false
    end.

is_fips(openssl, Config) ->
    VersionStr = proplists:get_value(openssl_version, Config),
    case re:split(VersionStr, "fips") of
	[_] ->
            case re:split(VersionStr, "FIPS") of
                [_] ->
                    false;
                _ ->
                    true
            end;
        _ ->
	    true
    end;
is_fips(crypto, _) ->
    [{_,_, Bin}]  = crypto:info_lib(),
    case re:split(Bin, <<"fips">>) of
	[_] ->
	    false;
	_ ->
	    true
    end.

%% Actual support is tested elsewhere, this is to exclude some LibreSSL and OpenSSL versions
openssl_sane_dtls() -> 
    case portable_cmd("openssl", ["version"]) of
        "OpenSSL 0." ++ _ ->
            false;
        "OpenSSL 1.0.1s-freebsd" ++ _ ->
            false;
        "OpenSSL 1.0.2k-freebsd" ++ _ ->
            false;
        "OpenSSL 1.0.2" ++ _ ->
            false;
        "OpenSSL 1.0.0" ++ _ ->
            false;
        "OpenSSL" ++ _ ->
            true;
        "LibreSSL 2.7" ++ _ ->
            true;
        _ ->
            false
        end.

check_sane_openssl_version(Version, Config) ->
    case supports_ssl_tls_version(Version, Config) of
	true ->
	    case {Version, proplists:get_value(openssl_version, Config)} of
                {'dtlsv1', "OpenSSL 0" ++ _} ->
		    false;
		{'dtlsv1.2', "OpenSSL 0" ++ _} ->
		    false;
                {'dtlsv1.2', "OpenSSL 1.0.2" ++ _} ->
		    false;
		{'dtlsv1',  "OpenSSL 1.0.0" ++ _} ->
		    false;
                {'dtlsv1', _} ->
		    not is_fips(openssl, Config);
		{'dtlsv1.2', _} ->
		    not is_fips(openssl, Config);
		{_, "OpenSSL 1.0.2" ++ _} ->
		    true;
		{_, "OpenSSL 1.0.1" ++ _} ->
		    true;
		{'tlsv1.2', "OpenSSL 1.0.0" ++ _} ->
		    false;
		{'tlsv1.1', "OpenSSL 1.0.0" ++ _} ->
		    false;
		{'tlsv1.2', "OpenSSL 0" ++ _} ->
		    false;
		{'tlsv1.1', "OpenSSL 0" ++ _} ->
		    false;
                {'tlsv1', "OpenSSL 0" ++ _} ->
		    false;
		{_, _} ->
		    true
	    end;
	false ->
	    false
    end.


%% If other DSA checks have passed also check the following
check_sane_openssl_dsa(Config) ->
    case not is_fips(openssl, Config) of
        true ->
            case proplists:get_value(openssl_version, Config) of
                "OpenSSL 1.0." ++ _ ->
                    false;
                _ ->
                    true
            end;
        false ->
            false
    end.

check_sane_openssl_renegotiate(Config, Version) when  Version == 'tlsv1';
                                                      Version == 'tlsv1.1';
                                                      Version == 'tlsv1.2' ->
    case proplists:get_value(openssl_version, Config) of
	"OpenSSL 1.0.1c" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 1.0.1b" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 1.0.1a" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 1.0.1 " ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
        "LibreSSL 3.0.2" ++ _ ->
	    {skip, "Known renegotiation bug in LibreSSL"};
        "LibreSSL 3.1" ++ _ ->
	    {skip, "Known renegotiation bug in LibreSSL"};
        _ ->
	    check_sane_openssl_renegotiate(Config)
    end;
check_sane_openssl_renegotiate(Config, _) ->
    check_sane_openssl_renegotiate(Config).

check_sane_openssl_renegotiate(Config) ->
    case proplists:get_value(openssl_version, Config) of
	"OpenSSL 1.0.0" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 0.9.8" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 0.9.7" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
        "LibreSSL 2." ++ _ ->
	    {skip, "Known renegotiation bug in LibreSSL"};
        "LibreSSL 3." ++ _ ->
	    {skip, "Known renegotiation bug in LibreSSL"};
	_ ->
	    Config
    end.

openssl_allows_client_renegotiate(Config) ->
    case proplists:get_value(openssl_version, Config) of
        "OpenSSL 3" ++ _ ->
            {skip, "OpenSSL does not allow client renegotiation"};
	"OpenSSL 1.1" ++ _ ->
	    {skip, "OpenSSL does not allow client renegotiation"};
	"LibreSSL" ++ _ ->
	    {skip, "LibreSSL does not allow client renegotiation"};
         _ ->
             Config
     end.

openssl_allows_server_renegotiate(Config) ->
    case proplists:get_value(openssl_version, Config) of
	"LibreSSL 3.1" ++ _ ->
	    {skip, "LibreSSL 3.1 does not allow server renegotiation"};
         _ ->
             Config
     end.


enough_openssl_crl_support("OpenSSL 0." ++ _) -> false;
enough_openssl_crl_support(_) -> true.

wait_for_openssl_server(Port, tls) ->
    do_wait_for_openssl_tls_server(Port, 10);
wait_for_openssl_server(_Port, dtls) ->
    ct:sleep(?SLEEP div 2),
    ok. %% No need to wait for DTLS over UDP server
        %% client will retransmitt until it is up.
        %% But wait a little for openssl debug printing


do_wait_for_openssl_tls_server(_, 0) ->
    exit(failed_to_connect_to_openssl);
do_wait_for_openssl_tls_server(Port, N) ->
    case gen_tcp:connect("localhost", Port, []) of
	{ok, S} ->
	    gen_tcp:close(S),
            ok;
	_  ->
	    ct:sleep(?SLEEP),
	    do_wait_for_openssl_tls_server(Port, N-1)
    end.

version_flag(tlsv1) ->
    "-tls1";
version_flag('tlsv1.1') ->
    "-tls1_1";
version_flag('tlsv1.2') ->
    "-tls1_2";
version_flag('tlsv1.3') ->
    "-tls1_3";
version_flag(sslv3) ->
    "-ssl3";
version_flag(sslv2) ->
    "-ssl2";
version_flag('dtlsv1.2') ->
    "-dtls1_2";
version_flag('dtlsv1') ->
    "-dtls1".

-define(OPENSSL_QUIT, "Q\n").
close_port(Port) ->
    catch port_command(Port, ?OPENSSL_QUIT),
    close_loop(Port, 500, false).

close_loop(Port, Time, SentClose) ->
    receive 
	{Port, {data,Debug}} when is_port(Port) ->
	    ?CT_LOG("openssl ~s~n",[Debug]),
	    close_loop(Port, Time, SentClose);	
	{ssl,_,Msg} ->
	    ?CT_LOG("ssl Msg ~s~n",[Msg]),
	    close_loop(Port, Time, SentClose);	
	{Port, closed} -> 
	    ?CT_LOG("Port Closed~n",[]),
	    ok;
	{'EXIT', Port, Reason} ->
	    ?CT_LOG("Port Closed ~p~n",[Reason]),
	    ok;
	Msg ->
	    ?CT_LOG("Port Msg ~p~n",[Msg]),
	    close_loop(Port, Time, SentClose)
    after Time ->
	    case SentClose of
		false -> 
		    ?CT_LOG("Closing port ~n",[]),
		    catch erlang:port_close(Port),
		    close_loop(Port, Time, true);
		true ->
		    ?CT_LOG("Timeout~n",[])
	    end
    end.

portable_open_port("openssl" = Exe, Args0) ->
    IsWindows = case os:type() of
                    {win32, _} -> true;
                    _ -> false
                end,
    case IsWindows andalso os:getenv("WSLENV") of
        false ->
            AbsPath = os:find_executable(Exe),
            ?CT_LOG("open_port({spawn_executable, ~p}, [stderr_to_stdout,~n {args, \"~s\"}]).",
		 [AbsPath, lists:join($\s, Args0)]),
            open_port({spawn_executable, AbsPath},
                      [{args, Args0}, stderr_to_stdout]);
	_ ->
	    %% I can't get the new windows version of openssl.exe to be stable
	    %% certain server tests are failing for no reason.
	    %% This is using "linux" openssl via wslenv

	    Translate = fun([_Drive|":/" ++ _ ]= Path) ->
				string:trim(os:cmd("wsl wslpath -u " ++ Path));
			   (Arg) ->
				Arg
			end,
	    Args1 = [Translate(Arg) || Arg <- Args0],
	    Args = ["/C","wsl","openssl"| Args1] ++ ["2>&1"],
            Cmd = case erlang:system_info(wordsize) of
                      8 -> os:find_executable("cmd");
                      4 -> filename:join(os:getenv("WINDIR"),"sysnative/cmd")
                  end,
            ?CT_LOG("open_port({spawn_executable, ~p}, [stderr_to_stdout,~n {args, \"~s\"}]).",
                    [Cmd, lists:join($\s, Args)]),
	    open_port({spawn_executable, Cmd},
		      [{args, Args}, stderr_to_stdout, hide])
    end;
portable_open_port(Exe, Args) ->
    AbsPath = os:find_executable(Exe),
    ?CT_LOG("open_port({spawn_executable, ~p}, [{args, ~p}, stderr_to_stdout]).", [AbsPath, Args]),
    open_port({spawn_executable, AbsPath},
	      [{args, Args}, stderr_to_stdout]).


portable_cmd("openssl", ["version"]) ->
    case get(openssl_version) of
        undefined ->
            Port = portable_open_port("openssl", ["version"]),
            Version = collect_port_data(Port),
            put(openssl_version, Version),
            Version;
        Version ->
            Version
    end;
portable_cmd(Exe, Args) ->
    Port = portable_open_port(Exe, Args),
    collect_port_data(Port).

collect_port_data(Port) ->
    collect_port_data(Port, []).

collect_port_data(Port, Acc) ->
    receive
         {Port, {data, Data}} ->
            maybe_collect_more_port_data(Port, Acc ++ Data)
    end.

maybe_collect_more_port_data(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            maybe_collect_more_port_data(Port, Acc ++ Data) 
    after 500 ->
            Acc
    end.

supports_ssl_tls_version(Version, Config) 
  when Version == sslv2;
       Version == sslv3 ->

    case ubuntu_legacy_support() of
        true ->   
            case proplists:get_value(openssl_version, Config) of
                "OpenSSL 1.0.1" ++ _ ->
                    Version =/= sslv2;
                "OpenSSL 1" ++ _ ->
                    false;
                %% Appears to be broken
                "OpenSSL 0.9.8.o" ++ _ -> 
                    false;
                _ ->
                    VersionFlag = version_flag(Version),
                    Exe = "openssl",
                    Args = ["s_client", VersionFlag],
                    [{trap_exit, Trap}] = process_info(self(), [trap_exit]),
                    process_flag(trap_exit, true),
                    Port = portable_open_port(Exe, Args),
                    Bool = do_supports_ssl_tls_version(Port, ""),
                    consume_port_exit(Port),
                    process_flag(trap_exit, Trap),
                    Bool
            end;
        false ->
            false             
    end;
supports_ssl_tls_version(Version, _) ->
    VersionFlag = version_flag(Version),
    Exe = "openssl",
    Args = ["s_client", VersionFlag],
    Port = portable_open_port(Exe, Args),
    do_supports_ssl_tls_version(Port, "").

do_supports_ssl_tls_version(Port, Acc) ->
    receive 
        {Port, {data, Data}} -> 
            case Acc ++ Data of
                "unknown option"  ++ _ ->
                    false;
                "s_client: Option unknown" ++ _->
                    false;
                "s_client: Unknown option: " ++ _->
                    false;
                Info when length(Info) >= 24 ->
                    ?CT_LOG("~p", [Info]),
                    true;
                _ ->
                    do_supports_ssl_tls_version(Port, Acc ++ Data)
            end
    after 1000 ->
            true                        
    end.

ubuntu_legacy_support() -> 
    case os:type() of
        {unix, linux} ->
            Issue = os:cmd("more /etc/issue"),
            case re:run(Issue, "Ubuntu 1[6-9]+", [global]) of
                nomatch ->
                    true;
                _ ->
                    false
            end;
        _ ->
            true
    end.       

ssl_options(Extra, Option, Config) ->
    ExtraOpts = proplists:get_value(Extra, Config, []),
    ExtraOpts ++ ssl_options(Option, Config).

ssl_options(Option, Config) when is_atom(Option) ->
    ProtocolOpts = proplists:get_value(protocol_opts, Config, []),
    Opts = proplists:get_value(Option, Config, []),
    Opts ++ ProtocolOpts;
ssl_options(Options, Config) ->
    ProtocolOpts = proplists:get_value(protocol_opts, Config, []),
    Options ++ ProtocolOpts.

protocol_version(Config) ->
   case proplists:get_value(version, Config, undefined) of
       undefined -> 
           protocol_version(Config, atom);
       Version ->
           Version
   end.
protocol_version(Config, tuple) ->
    case proplists:get_value(protocol, Config) of
	dtls ->
	    dtls_record:highest_protocol_version(dtls_record:supported_protocol_versions());
	_ ->
	    tls_record:highest_protocol_version(tls_record:supported_protocol_versions())
   end;

protocol_version(Config, atom) ->
    case proplists:get_value(protocol, Config) of
	dtls ->
	   dtls_record:protocol_version(protocol_version(Config, tuple));
	_ ->
            tls_record:protocol_version(protocol_version(Config, tuple))
   end.

protocol_options(Config, Options) ->
    Protocol = proplists:get_value(protocol, Config, tls),
    {Protocol, Opts} = lists:keyfind(Protocol, 1, Options),
    Opts.

ct_log_supported_protocol_versions(Config) ->
    case proplists:get_value(protocol, Config) of
	dtls ->
	    ?CT_LOG("DTLS version ~p~n ", [dtls_record:supported_protocol_versions()]);
	_ ->
	    ?CT_LOG("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()])
    end.

clean_env() ->
    application:unset_env(ssl, protocol_version),
    application:unset_env(ssl, dtls_protocol_version),
    application:unset_env(ssl, session_lifetime),
    application:unset_env(ssl, session_cb),
    application:unset_env(ssl, session_cb_init_args),
    application:unset_env(ssl, session_cache_client_max),
    application:unset_env(ssl, session_cache_server_max),
    application:unset_env(ssl, ssl_pem_cache_clean),
    application:unset_env(ssl, bypass_pem_cache),
    application:unset_env(ssl, alert_timeout),
    application:unset_env(ssl, internal_active_n).
%%
clean_env(keep_version) ->
    application:unset_env(ssl, session_lifetime),
    application:unset_env(ssl, session_cb),
    application:unset_env(ssl, session_cb_init_args),
    application:unset_env(ssl, session_cache_client_max),
    application:unset_env(ssl, session_cache_server_max),
    application:unset_env(ssl, ssl_pem_cache_clean),
    application:unset_env(ssl, bypass_pem_cache),
    application:unset_env(ssl, alert_timeout),
    application:unset_env(ssl, internal_active_n).

clean_start() ->
    ssl:stop(),
    application:load(ssl),
    clean_env(),
    ssl:start().
%%
clean_start(keep_version) ->
    ssl:stop(),
    application:load(ssl),
    clean_env(keep_version),
    ssl:start().


tls_version('dtlsv1' = Atom) ->
    dtls_v1:corresponding_tls_version(dtls_record:protocol_version_name(Atom));
tls_version('dtlsv1.2' = Atom) ->
    dtls_v1:corresponding_tls_version(dtls_record:protocol_version_name(Atom));
tls_version(Atom) ->
    tls_record:protocol_version_name(Atom).


n_version(Version) when
      Version == 'tlsv1.3';
      Version == 'tlsv1.2';
      Version == 'tlsv1.1';
      Version == 'tlsv1';
      Version == 'sslv3' ->
    tls_record:protocol_version_name(Version);
n_version(Version) when Version == 'dtlsv1.2';
                        Version == 'dtlsv1' ->
    dtls_record:protocol_version_name(Version).

consume_port_exit(OpenSSLPort) ->
    receive    	
        {'EXIT', OpenSSLPort, _} ->
            ok
    end.

hardcode_rsa_key(1) ->
    #'RSAPrivateKey'{
       version = 'two-prime',
       modulus = 23995666614853919027835084074500048897452890537492185072956789802729257783422306095699263934587064480357348855732149402060270996295002843755712064937715826848741191927820899197493902093529581182351132392364214171173881547273475904587683433713767834856230531387991145055273426806331200574039205571401702219159773947658558490957010003143162250693492642996408861265758000254664396313741422909188635443907373976005987612936763564996605457102336549804831742940035613780926178523017685712710473543251580072875247250504243621640157403744718833162626193206685233710319205099867303242759099560438381385658382486042995679707669,
       publicExponent = 17,
       privateExponent = 11292078406990079542510627799764728892919007311761028269626724613049062486316379339152594792746853873109340637991599718616598115903530750002688030558925094987642913848386305504703012749896273497577003478759630198199473669305165131570674557041773098755873191241407597673069847908861741446606684974777271632545629600685952292605647052193819136445675100211504432575554351515262198132231537860917084269870590492135731720141577986787033006338680118008484613510063003323516659048210893001173583018220214626635609151105287049126443102976056146630518124476470236027123782297108342869049542023328584384300970694412006494684657,
       prime1 = 169371138592582642967021557955633494538845517070305333860805485424261447791289944610138334410987654265476540480228705481960508520379619587635662291973699651583489223555422528867090299996446070521801757353675026048850480903160224210802452555900007597342687137394192939372218903554801584969667104937092080815197,
       prime2 = 141675062317286527042995673340952251894209529891636708844197799307963834958115010129693036021381525952081167155681637592199810112261679449166276939178032066869788822014115556349519329537177920752776047051833616197615329017439297361972726138285974555338480581117881706656603857310337984049152655480389797687577,
       exponent1 = 119556097830058336212015217380447172615655659108450823901745048534772786676204666783627059584226579481512852103690850928442711896738555003036938088452023283470698275450886490965004917644550167427154181661417665446247398284583687678213495921811770068712485038160606780733330990744565824684470897602653233516609,
       exponent2 = 41669135975672507953822256864985956439473391144599032012999352737636422046504414744027363535700448809435637398729893409470532385959317485048904982111185902020526124121798693043976273393287623750816484427009887116945685005129205106462566511260580751570141347387612266663707016855981760014456663376585234613993,
       coefficient = 76837684977089699359024365285678488693966186052769523357232308621548155587515525857011429902602352279058920284048929101483304120686557782043616693940283344235057989514310975192908256494992960578961614059245280827077951132083993754797053182279229469590276271658395444955906108899267024101096069475145863928441,
       otherPrimeInfos = asn1_NOVALUE};

hardcode_rsa_key(2) ->
    #'RSAPrivateKey'{
       version = 'two-prime',
       modulus = 21343679768589700771839799834197557895311746244621307033143551583788179817796325695589283169969489517156931770973490560582341832744966317712674900833543896521418422508485833901274928542544381247956820115082240721897193055368570146764204557110415281995205343662628196075590438954399631753508888358737971039058298703003743872818150364935790613286541190842600031570570099801682794056444451081563070538409720109449780410837763602317050353477918147758267825417201591905091231778937606362076129350476690460157227101296599527319242747999737801698427160817755293383890373574621116766934110792127739174475029121017282777887777,
       publicExponent = 17,
       privateExponent = 18832658619343853622211588088997845201745658451136447382185486691577805721584993260814073385267196632785528033211903435807948675951440868570007265441362261636545666919252206383477878125774454042314841278013741813438699754736973658909592256273895837054592950290554290654932740253882028017801960316533503857992358685308186680144968293076156011747178275038098868263178095174694099811498968993700538293188879611375604635940554394589807673542938082281934965292051746326331046224291377703201248790910007232374006151098976879987912446997911775904329728563222485791845480864283470332826504617837402078265424772379987120023773,
       prime1 = 146807662748886761089048448970170315054939768171908279335181627815919052012991509112344782731265837727551849787333310044397991034789843793140419387740928103541736452627413492093463231242466386868459637115999163097726153692593711599245170083315894262154838974616739452594203727376460632750934355508361223110419,
       prime2 = 145385325050081892763917667176962991350872697916072592966410309213561884732628046256782356731057378829876640317801978404203665761131810712267778698468684631707642938779964806354584156202882543264893826268426566901882487709510744074274965029453915224310656287149777603803201831202222853023280023478269485417083,
       exponent1 = 51814469205489445090252393754177758254684624060673510353593515699736136004585238510239335081623236845018299924941168250963996835808180162284853901555621683602965806809675350150634081614988136541809283687999704622726877773856604093851236499993845033701707873394143336209718962603456693912094478414715725803677,
       exponent2 = 51312467664734785681382706062457526359131540440966797517556579722433606376221663384746714140373192528191755406283051201483646739222992016094510128871300458249756331334105225772206172777487956446433115153562317730076172132768497908567634716277852432109643395464627389577600646306666889302334125933506877206029,
       coefficient = 30504662229874176232343608562807118278893368758027179776313787938167236952567905398252901545019583024374163153775359371298239336609182249464886717948407152570850677549297935773605431024166978281486607154204888016179709037883348099374995148481968169438302456074511782717758301581202874062062542434218011141540,
       otherPrimeInfos = asn1_NOVALUE};
hardcode_rsa_key(3) -> 
    #'RSAPrivateKey'{ 
       version = 'two-prime',
       modulus = 25089040456112869869472694987833070928503703615633809313972554887193090845137746668197820419383804666271752525807484521370419854590682661809972833718476098189250708650325307850184923546875260207894844301992963978994451844985784504212035958130279304082438876764367292331581532569155681984449177635856426023931875082020262146075451989132180409962870105455517050416234175675478291534563995772675388370042873175344937421148321291640477650173765084699931690748536036544188863178325887393475703801759010864779559318631816411493486934507417755306337476945299570726975433250753415110141783026008347194577506976486290259135429,
       publicExponent = 17,
       privateExponent = 8854955455098659953931539407470495621824836570223697404931489960185796768872145882893348383311931058684147950284994536954265831032005645344696294253579799360912014817761873358888796545955974191021709753644575521998041827642041589721895044045980930852625485916835514940558187965584358347452650930302268008446431977397918214293502821599497633970075862760001650736520566952260001423171553461362588848929781360590057040212831994258783694027013289053834376791974167294527043946669963760259975273650548116897900664646809242902841107022557239712438496384819445301703021164043324282687280801738470244471443835900160721870265,
       prime1 = 171641816401041100605063917111691927706183918906535463031548413586331728772311589438043965564336865070070922328258143588739626712299625805650832695450270566547004154065267940032684307994238248203186986569945677705100224518137694769557564475390859269797990555863306972197736879644001860925483629009305104925823,
       prime2 =146170909759497809922264016492088453282310383272504533061020897155289106805616042710009332510822455269704884883705830985184223718261139908416790475825625309815234508695722132706422885088219618698987115562577878897003573425367881351537506046253616435685549396767356003663417208105346307649599145759863108910523,
       exponent1 = 60579464612132153154728441333538327425711971378777222246428851853999433684345266860486105493295364142377972586444050678378691780811632637288529186629507258781295583787741625893888579292084087601124818789392592131211843947578009918667375697196773859928702549128225990187436545756706539150170692591519448797349,
       exponent2 = 137572620950115585809189662580789132500998007785886619351549079675566218169991569609420548245479957900898715184664311515467504676010484619686391036071176762179044243478326713135456833024206699951987873470661533079532774988581535389682358631768109586527575902839864474036157372334443583670210960715165278974609,
       coefficient = 15068630434698373319269196003209754243798959461311186548759287649485250508074064775263867418602372588394608558985183294561315208336731894947137343239541687540387209051236354318837334154993136528453613256169847839789803932725339395739618592522865156272771578671216082079933457043120923342632744996962853951612,
       otherPrimeInfos = asn1_NOVALUE};
hardcode_rsa_key(4) -> 
    #'RSAPrivateKey'{
       version ='two-prime',
       modulus = 28617237755030755643854803617273584643843067580642149032833640135949799721163782522787597288521902619948688786051081993247908700824196122780349730169173433743054172191054872553484065655968335396052034378669869864779940355219732200954630251223541048434478476115391643898092650304645086338265930608997389611376417609043761464100338332976874588396803891301015812818307951159858145399281035705713082131199940309445719678087542976246147777388465712394062188801177717719764254900022006288880246925156931391594131839991579403409541227225173269459173129377291869028712271737734702830877034334838181789916127814298794576266389,
       publicExponent = 17,
       privateExponent = 26933870828264240605980991639786903194205240075898493207372837775011576208154148256741268036255908348187001210401018346586267012540419880263858569570986761169933338532757527109161473558558433313931326474042230460969355628442100895016122589386862163232450330461545076609969553227901257730132640573174013751883368376011370428995523268034111482031427024082719896108094847702954695363285832195666458915142143884210891427766607838346722974883433132513540317964796373298134261669479023445911856492129270184781873446960437310543998533283339488055776892320162032014809906169940882070478200435536171854883284366514852906334641,
       prime1 = 177342190816702392178883147766999616783253285436834252111702533617098994535049411784501174309695427674025956656849179054202187436663487378682303508229883753383891163725167367039879190685255046547908384208614573353917213168937832054054779266431207529839577747601879940934691505396807977946728204814969824442867,
       prime2 = 161367340863680900415977542864139121629424927689088951345472941851682581254789586032968359551717004797621579428672968948552429138154521719743297455351687337112710712475376510559020211584326773715482918387500187602625572442687231345855402020688502483137168684570635690059254866684191216155909970061793538842967,
       exponent1 = 62591361464718491357252875682470452982324688977706206627659717747211409835899792394529826226951327414362102349476180842659595565881230839534930649963488383547255704844176717778780890830090016428673547367746320007264898765507470136725216211681602657590439205035957626212244060728285168687080542875871702744541,
       exponent2 = 28476589564178982426348978152495139111074987239250991413906989738532220221433456358759122273832412611344984605059935696803369847909621479954699550944415412431654831613301737157474154985469430655673456186029444871051571607533040825739188591886206320553618003159523945304574388238386685203984112363845918619347,
       coefficient = 34340318160575773065401929915821192439103777558577109939078671096408836197675640654693301707202885840826672396546056002756167635035389371579540325327619480512374920136684787633921441576901246290213545161954865184290700344352088099063404416346968182170720521708773285279884132629954461545103181082503707725012,
       otherPrimeInfos = asn1_NOVALUE};

hardcode_rsa_key(5) -> 
    #'RSAPrivateKey'{ 
       version= 'two-prime',
       modulus = 26363170152814518327068346871197765236382539835597898797762992537312221863402655353436079974302838986536256364057947538018476963115004626096654613827403121905035011992899481598437933532388248462251770039307078647864188314916665766359828262009578648593031111569685489178543405615478739906285223620987558499488359880003693226535420421293716164794046859453204135383236667988765227190694994861629971618548127529849059769249520775574008363789050621665120207265361610436965088511042779948238320901918522125988916609088415989475825860046571847719492980547438560049874493788767083330042728150253120940100665370844282489982633,
       publicExponent = 17,
       privateExponent = 10855423004100095781734025182257903332628104638187370093196526338893267826106975733767797636477639582691399679317978398007608161282648963686857782164224814902073240232370374775827384395689278778574258251479385325591136364965685903795223402003944149420659869469870495544106108194608892902588033255700759382142132115013969680562678811046675523365751498355532768935784747314021422035957153013494814430893022253205880275287307995039363642554998244274484818208792520243113824379110193356010059999642946040953102866271737127640405568982049887176990990501963784502429481034227543991366980671390566584211881030995602076468001,
       prime1 =163564135568104310461344551909369650951960301778977149705601170951529791054750122905880591964737953456660497440730575925978769763154927541340839715938951226089095007207042122512586007411328664679011914120351043948122025612160733403945093961374276707993674792189646478659304624413958625254578122842556295400709,
       prime2 = 161179405627326572739107057023381254841260287988433675196680483761672455172873134522398837271764104320975746111042211695289319249471386600030523328069395763313848583139553961129874895374324504709512019736703349829576024049432816885712623938437949550266365056310544300920756181033500610331519029869549723159637,
       exponent1 = 115457036871603042678596154288966812436677860079277988027483179495197499568058910286503947269226790675289762899339230065396778656344654735064122152427494983121714122734382674714766593466820233891067233496718383963380253373289929461608301619793607087995535147427985749641862087821617853120878674947686796753441,
       exponent2 = 142217122612346975946270932667689342506994371754500301644129838613240401623123353990351915239791856753802128921507833848784693455415929352968108818884760967629866396887841730408713142977345151214275311532385308673155315337734838428569962298621720191411498579097539089047726042088382891468987379296661520434973,
       coefficient = 40624877259097915043489529504071755460170951428490878553842519165800720914888257733191322215286203357356050737713125202129282154441426952501134581314792133018830748896123382106683994268028624341502298766844710276939303555637478596035491641473828661569958212421472263269629366559343208764012473880251174832392,
       otherPrimeInfos = asn1_NOVALUE};
hardcode_rsa_key(6) -> 
    #'RSAPrivateKey'{ 
       version = 'two-prime',
       modulus = 22748888494866396715768692484866595111939200209856056370972713870125588774286266397044592487895293134537316190976192161177144143633669641697309689280475257429554879273045671863645233402796222694405634510241820106743648116753479926387434021380537483429927516962909367257212902212159798399531316965145618774905828756510318897899298783143203190245236381440043169622358239226123652592179006905016804587837199618842875361941208299410035232803124113612082221121192550063791073372276763648926636149384299189072950588522522800393261949880796214514243704858378436010975184294077063518776479282353562934591448646412389762167039,
       publicExponent = 17,
       privateExponent = 6690849557313646092873144848490175032923294179369428344403739373566349639495960705013115437616262686628622409110644753287395336362844012263914614494257428655751435080307550548130951000822418439531068973600535325512837681398082331290421770994275730420566916753796872722709677121223470117509210872101652580854566448661533030419787125312956120661097410038933324613372774190658239039998357548275441758790939430824924502690997433186652165055694361752689819209062683281242276039100201318203707142383491769671330743466041394101421674581185260900666085723130684175548215193875544802254923825103844262661010117443222587769713,
       prime1 = 164748737139489923768181260808494855987398781964531448608652166632780898215212977127034263859971474195908846263894581556691971503119888726148555271179103885786024920582830105413607436718060544856016793981261118694063993837665813285582095833772675610567592660039821387740255651489996976698808018635344299728063,
       prime2 = 138082323967104548254375818343885141517788525705334488282154811252858957969378263753268344088034079842223206527922445018725900110643394926788280539200323021781309918753249061620424428562366627334409266756720941754364262467100514166396917565961434203543659974860389803369482625510495464845206228470088664021953,
       exponent1 = 19382204369351755737433089506881747763223386113474288071606137250915399790025056132592266336467232258342217207517009594904937823896457497193947678962247515974826461245038835931012639613889475865413740468383661022831058098548919210068481862796785365949128548239978986792971253116470232552800943368864035262125,
       exponent2 = 48734937870742781736838524121371226418043009072470995864289933383361985165662916618800592031070851709019955245149098241903258862580021738866451955011878713569874088971734962924855680669070574353320917678842685325069739694270769705787147376221682660074232932303666989424523279591939575827719845342384234360689,
       coefficient = 81173034184183681160439870161505779100040258708276674532866007896310418779840630960490793104541748007902477778658270784073595697910785917474138815202903114440800310078464142273778315781957021015333260021813037604142367434117205299831740956310682461174553260184078272196958146289378701001596552915990080834227,
       otherPrimeInfos = asn1_NOVALUE}.

hardcode_rsa_1024_key(1) ->
    #'RSAPrivateKey'{version = 'two-prime',
                     modulus = 152618920709346576506952607098028299458615405194120516804067302859774798720862572082626851690572130284910454988859007980367926204341637028795420927026111160369130942718840998292351565050537705794496742217762844103737634290634532232714374862322877076125650783658974242305324207239909234718160759907957502819181,
                     publicExponent = 17,
                     privateExponent = 89775835711380339121736827704722529093303179525953245178863119329279293365213277695662853935630664873476738228740592929628191884906845311056129957074183020957315463095429495020547731127789657232144654051871515007759243605000909583210051114028049068215595185959728886310943042856399988846590947179831354428913,
                     prime1 = 13018105310773689694711101111767176661493882304979760063552973933059514785910240943852845097923711145970844208861778343060919395218474310542285865516544653,
                     prime2 = 11723589344682921162046319310366118627005968525349821205037572987102618200031016344115630095736447992996683226273798377973464634035204645607416378683745377,
                     exponent1 = 7657709006337464526300647712804221565584636649988094155031161137093832227006024084619320645837477144688731887565751966506423173657926065024874038539143913,
                     exponent2 = 11033966442054514034867124056815170472476205670917478781211833399625993600029191853285298913634303993408643036492986708680907890856663195865803650525878001,
                     coefficient = 7357357483264399363785138527396251818499941660605442417644885251395376792981387533016821796011101917057636813775613592220898054882923958484000235934554630,
                     otherPrimeInfos = asn1_NOVALUE};
hardcode_rsa_1024_key(2) ->
    #'RSAPrivateKey'{version = 'two-prime',
                     modulus = 132417984874904377472239436563253515498607309816574784497785056464473431603604973287322746340055062696030016903830406088140534281534301418467490242269156926775506208514027213826501153438861284871625076651352798208559277520683414148048437439635357639033850360133068980157555507518934285770383924814915583919331,
                     publicExponent = 17,
                     privateExponent = 116839398419033274240211267555811925439947626308742456909810343939241263179651447018225952652989761202379426679850358313065177307236148310412491390237491385620149263549211570156731410125598364338974865883306073709062002620705336269289633237348474049621806833904576124689232282666798030505410189805859996211233,
                     prime1 = 12354286715326546399270830019697416039683060665495490476376955068446562229853736822465010796530936501225722243114286822522048306078247961653481711526701259,
                     prime2 = 10718383661165041035044708868932433765604392896488115438294667272770655136522450030638962957185192634722652306257889603065114923772949624056219896061512009,
                     exponent1 = 5087059235722695576170341772816583075163613215204025490272863851713290329939773985720886798571562088740003276576471044567902243679278572445551292981582871,
                     exponent2 = 6304931565391200608849828746430843391531995821463597316643921925159208903836735312140566445403054491013324886034052707685361719866440955327188174153830593,
                     coefficient = 6764088858264512915296172980190092445938774052616013205418164952211827027745702759906572599388571087295432259160097016323193144471211837074613329649320009,
                     otherPrimeInfos = asn1_NOVALUE};
hardcode_rsa_1024_key(3) ->
    #'RSAPrivateKey'{version = 'two-prime',
                     modulus = 132603582566987335579015215397416921308461253540735107996254563101087328483405996961761145905021132317760270172654141110354018131670337351296871719192630978670273323069438897632586026697023844069174787494970866246368200405578784055149230641370998125414763230872277095376893138420738507940599560410343688278361,
                     publicExponent = 17,
                     privateExponent = 124803371827752786427308438021098278878551768038338925172945471153964544454970350081657549087078712769656724868380368103862605300395611624749996912181299722918452562565562892031863847812293655197586374503957768862684015202213024002730410420619423541154205461118764880018581745374583581669240937327152309672753,
                     prime1 = 12202483472094988277172439292742673588688995751099198683383744575043357099902468606144011463115716181768777309695574163698153032647393450605174909802187971,
                     prime2 = 10866934003248540047676291395653788246732743513485317053446021859209870346149779563425451397497222238159656279714782986335807210805023580459325334557063091,
                     exponent1 = 3588965727086761257991893909630198114320292867970352553936395463248046205853667237101179842092857700520228620498698283440633244896292191354463208765349403,
                     exponent2 = 4474619883690575313749061162916265748654659093788071727889538412615828966061673937881068222498856215712799644588440053197097086802068533130310431876437743,
                     coefficient = 6440880395775803235356940314241907933534073137546236980469653455119937607298142560546736915150573386382326185901566797818281064505978928392351326571984856,
                     otherPrimeInfos = asn1_NOVALUE}.


hardcode_dsa_key(1) -> 
    {'DSAPrivateKey',0,
     99438313664986922963487511141216248076486724382260996073922424025828494981416579966171753999204426907349400798052572573634137057487829150578821328280864500098312146772602202702021153757550650696224643730869835650674962433068943942837519621267815961566259265204876799778977478160416743037274938277357237615491,
     1454908511695148818053325447108751926908854531909,
     20302424198893709525243209250470907105157816851043773596964076323184805650258390738340248469444700378962907756890306095615785481696522324901068493502141775433048117442554163252381401915027666416630898618301033737438756165023568220631119672502120011809327566543827706483229480417066316015458225612363927682579,
     48598545580251057979126570873881530215432219542526130654707948736559463436274835406081281466091739849794036308281564299754438126857606949027748889019480936572605967021944405048011118039171039273602705998112739400664375208228641666852589396502386172780433510070337359132965412405544709871654840859752776060358,
     1457508827177594730669011716588605181448418352823};
hardcode_dsa_key(2) -> 
    #'DSAPrivateKey'{
       version = 0,
       p = 145447354557382582722944332987784622105075065624518040072393858097520305927329240484963764783346271194321683798321743658303478090647837211867389721684646254999291098347011037298359107547264573476540026676832159205689428125157386525591130716464335426605521884822982379206842523670736739023467072341958074788151,
       q = 742801637799670234315651916144768554943688916729,
       g = 79727684678125120155622004643594683941478642656111969487719464672433839064387954070113655822700268007902716505761008423792735229036965034283173483862273639257533568978482104785033927768441235063983341565088899599358397638308472931049309161811156189887217888328371767967629005149630676763492409067382020352505,
       y = 35853727034965131665219275925554159789667905059030049940938124723126925435403746979702929280654735557166864135215989313820464108440192507913554896358611966877432546584986661291483639036057475682547385322659469460385785257933737832719745145778223672383438466035853830832837226950912832515496378486927322864228,
       x = 801315110178350279541885862867982846569980443911};
hardcode_dsa_key(3) -> 
    #'DSAPrivateKey'{
       version = 0,
       p =  99438313664986922963487511141216248076486724382260996073922424025828494981416579966171753999204426907349400798052572573634137057487829150578821328280864500098312146772602202702021153757550650696224643730869835650674962433068943942837519621267815961566259265204876799778977478160416743037274938277357237615491,
       q =  1454908511695148818053325447108751926908854531909,
       g =  20302424198893709525243209250470907105157816851043773596964076323184805650258390738340248469444700378962907756890306095615785481696522324901068493502141775433048117442554163252381401915027666416630898618301033737438756165023568220631119672502120011809327566543827706483229480417066316015458225612363927682579,
       y =  48598545580251057979126570873881530215432219542526130654707948736559463436274835406081281466091739849794036308281564299754438126857606949027748889019480936572605967021944405048011118039171039273602705998112739400664375208228641666852589396502386172780433510070337359132965412405544709871654840859752776060358,
       x = 1457508827177594730669011716588605181448418352823}.


client_msg(Client, ClientMsg) ->
    receive
	{Client, ClientMsg} ->
	    ok;
	{Client, {error,closed}} ->
	    ?CT_LOG("client got close", []),
	    ok;
	{Client, {error, Reason}} ->
	    ?CT_LOG("client got econnaborted: ~p", [Reason]),
	    ok;
	Unexpected ->
	    ct:fail(Unexpected)
    end.
server_msg(Server, ServerMsg) ->
    receive
	{Server, ServerMsg} ->
	    ok;
	{Server, {error,closed}} ->
	    ?CT_LOG("server got close", []),
	    ok;
	{Server, {error, Reason}} ->
	    ?CT_LOG("server got econnaborted: ~p", [Reason]),
	    ok;
	Unexpected ->
	    ct:fail(Unexpected)
    end.

session_id(Socket) ->
    {ok, [{session_id, ID}]} = ssl:connection_information(Socket, [session_id]),
    ID.
    
reuse_session(ClientOpts, ServerOpts, Config) ->
    {ClientNode, ServerNode, Hostname} = run_where(Config),

    Server0 =
	start_server([{node, ServerNode}, {port, 0},
                      {from, self()},
                      {mfa, {ssl_test_lib, no_result, []}},
                      {tcp_options, [{active, false}]},
                      {options, ServerOpts}]),
    Port0 = inet_port(Server0),

    Client0 = start_client([{node, ClientNode},
                            {port, Port0}, {host, Hostname},
                            {mfa, {ssl_test_lib, session_id, []}},
                            {from, self()},  {options, [{reuse_sessions, save} | ClientOpts]}]),
    Server0 ! listen,

    SID = receive
              {Client0, Id0} ->
                  Id0
          end,

    Client1 = start_client([{node, ClientNode},
                            {port, Port0}, {host, Hostname},
                            {mfa, {ssl_test_lib, session_id, []}},
                            {from, self()},  {options, ClientOpts}]),

    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
            ct:fail(session_not_reused)
    end,

    Server0 ! listen,

    Client2 =
        start_client([{node, ClientNode},
                      {port, Port0}, {host, Hostname},
                      {mfa, {ssl_test_lib, session_id, []}},
                      {from, self()},  {options, [{reuse_sessions, false}
                                                 | ClientOpts]}]),   
    receive
        {Client2, SID} ->
            ct:fail(session_reused_when_session_reuse_disabled_by_client);
        {Client2, _} ->
            ok
    end,

    close(Server0),
    close(Client0),
    close(Client1),
    close(Client2),

    Server1 =
	start_server([{node, ServerNode}, {port, 0},
                      {from, self()},
                      {mfa, {ssl_test_lib, no_result, []}},
                      {tcp_options, [{active, false}]},
                      {options, [{reuse_sessions, false} | ServerOpts]}]),
    Port1 = inet_port(Server1),

    Client3 = start_client([{node, ClientNode},
                            {port, Port1}, {host, Hostname},
                            {mfa, {ssl_test_lib, session_id, []}},
                            {from, self()},  {options, [{reuse_sessions, save} | ClientOpts]}]),
    SID1 = receive
               {Client3, Id3} ->
                   Id3
           end,

    Server1 ! listen,

    Client4 =
        start_client([{node, ClientNode},
                      {port, Port1}, {host, Hostname},
                      {mfa, {ssl_test_lib, session_id, []}},
                      {from, self()},  {options, ClientOpts}]),

    receive
        {Client4, SID1} ->
            ct:fail(session_reused_when_session_reuse_disabled_by_server);
        {Client4, _} ->
            ok
    end,

    close(Server1),
    close(Client3),
    close(Client4).

user_lookup(psk, _Identity, UserState) ->
    {ok, UserState};
user_lookup(srp, Username, _UserState) ->
    Salt = ssl_cipher:random_bytes(16),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, <<"secret">>])]),
    {ok, {srp_1024, Salt, UserPassHash}}.

test_cipher(TestCase, Config) ->
    [{name, Group} |_] = proplists:get_value(tc_group_properties, Config),
    list_to_atom(re:replace(atom_to_list(TestCase), atom_to_list(Group) ++ "_",  "", [{return, list}])).

digest() ->
    case application:get_env(ssl, protocol_version, application:get_env(ssl, dtls_protocol_version)) of
        Ver when Ver == 'tlsv1.2';
                 Ver == 'dtlsv1.2' ->
            {digest, sha256};
        _ ->
            {digest, sha1}
    end.

kill_openssl() ->
    case os:type() of
        {win32, _} ->
            case os:getenv("WSLENV") of
                false -> os:cmd("cmd.exe /C \"taskkill /IM openssl.exe /F\"");
                _ -> os:cmd("wsl pkill openssl")
            end;
        _ ->
            os:cmd("pkill openssl")
    end.

hostname_format(Hostname) ->
    case lists:member($., Hostname) of
        true ->  
            Hostname;
        false ->
            "localhost"   
    end.

erlang_ssl_receive_and_assert_negotiated_protocol(Socket, Protocol, Data) ->
    case ssl:negotiated_protocol(Socket) of
        {ok, Protocol} ->
            active_recv(Socket, length(Data));
        Result ->
            {error, {{expected, Protocol}, {got, Result}}}
    end. 

check_openssl_npn_support(Config) ->
    case proplists:get_value(openssl_version, Config) of
        "OpenSSL 1.0"  ++ _  ->
            false;
        "OpenSSL 1.1" ++ _ ->
            true;
        _ ->
            false
     end.

new_config(PrivDir, ServerOpts0) ->
    CaCertFile = proplists:get_value(cacertfile, ServerOpts0),
    CertFile = proplists:get_value(certfile, ServerOpts0),
    KeyFile = proplists:get_value(keyfile, ServerOpts0),
    NewCaCertFile = filename:join(PrivDir, "new_ca.pem"),
    NewCertFile = filename:join(PrivDir, "new_cert.pem"),
    NewKeyFile = filename:join(PrivDir, "new_key.pem"),
    file:copy(CaCertFile, NewCaCertFile),
    file:copy(CertFile, NewCertFile),
    file:copy(KeyFile, NewKeyFile),
    ServerOpts1 = proplists:delete(cacertfile, ServerOpts0),
    ServerOpts2 = proplists:delete(certfile, ServerOpts1),
    ServerOpts = proplists:delete(keyfile, ServerOpts2),

    {ok, PEM} = file:read_file(NewCaCertFile),
    ?CT_LOG("CA file content: ~p~n", [public_key:pem_decode(PEM)]),

    [{cacertfile, NewCaCertFile}, {certfile, NewCertFile},
     {keyfile, NewKeyFile} | ServerOpts].


openssl_sane_dtls_alpn() ->
    case portable_cmd("openssl", ["version"]) of
        "OpenSSL 1.1.0g" ++ _ ->
            false;
        "OpenSSL 1.1.1 " ++ _ ->
            false;
        "OpenSSL 1.1.1a" ++ _ ->
            false;
        "OpenSSL 1.1.1d FIPS" ++ _ ->
            false;
        "OpenSSL 1.1.1d-freebsd" ++ _ ->
            false;
        _->
            openssl_sane_dtls()
    end.

openssl_sane_dtls_session_reuse() ->
    case portable_cmd("openssl", ["version"]) of
        "OpenSSL 1.1.1 " ++ _ ->
            false;
        "OpenSSL 1.1.1a" ++ _ ->
            false;
        _->
            openssl_sane_dtls()
    end.

set_protocol_versions(Version) when Version == 'tlsv1';
                                    Version == 'tlsv1.1';
                                    Version == 'tlsv1.2';
                                    Version == 'tlsv1.3'->    
    set_protocol_versions(protocol_version, [Version]);
set_protocol_versions(Version) when Version == 'dtlsv1';
                                    Version == 'dtlsv1.2' ->    
    set_protocol_versions(dtls_protocol_version, [Version]).

set_protocol_versions(_, undefined) ->
    ok;
set_protocol_versions(AppVar, Value) ->
    application:set_env(ssl, AppVar, Value).

pss_params(sha256) ->
    #'RSASSA-PSS-params'{
       hashAlgorithm = #'HashAlgorithm'{algorithm = ?'id-sha256'},
       maskGenAlgorithm = #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                              parameters = #'HashAlgorithm'{algorithm = ?'id-sha256'}
                                             },
       saltLength = 32,
       trailerField = 1}.
       
test_ciphers(Kex, Cipher, Version) ->
    ssl:filter_cipher_suites(
        ssl:cipher_suites(all, Version) ++ ssl:cipher_suites(anonymous, Version),
        [{key_exchange,
          fun(K) when K == Kex -> true;
             (_) -> false
          end},
         {cipher,
          fun(C) when C == Cipher -> true;
             (_) -> false
          end}]).

sanity_check(ErlangPeer, OpenSSLPort) ->
    Data = "OpenSSL to Erlang",
    port_command(OpenSSLPort, Data, [nosuspend]),
    Data = check_active_receive(ErlangPeer, Data).

default_tls_version(Config) ->
    case proplists:get_value(protocol, Config, tls) of
        tls ->
            {ok, Versions} = application:get_env(ssl, protocol_version),
            Versions;
        dtls ->
            {ok, Versions} = application:get_env(ssl, dtls_protocol_version),
            Versions
    end.

openssl_maxfraglen_support() ->
    case portable_cmd("openssl", ["version"]) of
        %% Max fragmentation support introduced in OpenSSL 1.1.1
        "OpenSSL 0" ++ _  ->
            false;
        "OpenSSL 1.0" ++ _  ->
            false;
        "OpenSSL 1.1.0" ++ _ ->
            false;
	"OpenSSL 1.1.1" ++ _ ->
            true;
        "OpenSSL 3.0" ++ _ ->
            false; %% OpenSSL sends internal error alert
        "OpenSSL" ++ _ ->
            true;
        _  ->
            false
    end.

assert_mfl(Socket, undefined) ->
    InfoMFL = ssl:connection_information(Socket, [max_fragment_length]),
    ?CT_LOG("Connection MFL ~p, Expecting: [] ~n", [InfoMFL]),
    {ok, []} = InfoMFL;
assert_mfl(Socket, MFL) ->
    InfoMFL = ssl:connection_information(Socket, [max_fragment_length]),
    ?CT_LOG("Connection MFL ~p, Expecting: ~p ~n", [InfoMFL, MFL]),
    {ok, [{max_fragment_length, ConnMFL}]} = InfoMFL,
    ConnMFL = MFL.
-define(BIG_BUF, 10000000).
%% Workaround data delivery issues on solaris | openbsd  when kernel buffers are small
bigger_buffers() ->
    case os:type() of
        {unix,sunos} ->
            [{buffer, ?BIG_BUF}, {recbuf, ?BIG_BUF},{sndbuf, ?BIG_BUF}];
        {unix,openbsd} ->
            [{buffer, ?BIG_BUF}, {recbuf, ?BIG_BUF},{sndbuf, ?BIG_BUF}];
        _ ->
            []
    end.

default_ciphers(Version) ->
    OpenSSLCiphers = openssl_ciphers(),
    Ciphers = 
        case portable_cmd("openssl", ["version"]) of
            "OpenSSL 0.9" ++ _ ->
                ssl:cipher_suites(all,Version);
            "OpenSSL 3." ++ _ ->
                ssl:filter_cipher_suites(ssl:cipher_suites(default, Version),
                                         [{mac,
                                           fun(sha) ->
                                                   false;
                                              (_) ->
                                                   true
                                           end}]);
            _ ->
                ssl:cipher_suites(default, Version)
        end, 
    [Cipher || Cipher <- Ciphers, lists:member(ssl:suite_to_openssl_str(Cipher), OpenSSLCiphers)].

verify_early_data(Atom) ->
    receive
        {ssl, _Socket, {early_data, Atom}} ->
            ok;
        {ssl, _Socket, {early_data, Other}} ->
            Other
    end.

curve_default(eddsa) ->
    ed25519;
curve_default(_) ->
    ?DEFAULT_CURVE.

trace() ->
    ssl_trace:start(fun ct:pal/2, []),
    ssl_trace:on().

handle_trace(rle,
                 {call, {?MODULE, init_openssl_server, [Mode, ResponderPort | _]}}, Stack0) ->
    Role = server,
    {io_lib:format("(*~w) Mode = ~w ResponderPort = ~w",
                   [Role, Mode, ResponderPort]),
     [{role, Role} | Stack0]}.


ktls_os() ->
    inet_tls_dist:ktls_os().

%% Set UserLand Protocol
ktls_set_ulp(Socket, OS) ->
    inet_tls_dist:set_ktls_ulp(
      #{ socket => Socket,
         setopt_fun => fun inet_tls_dist:inet_ktls_setopt/3,
         getopt_fun => fun inet_tls_dist:inet_ktls_getopt/3 },
      OS).

ktls_set_cipher(Socket, OS, TxRx, Seed) ->
    TLS_version = ?TLS_1_3,
    TLS_cipher = ?TLS_AES_256_GCM_SHA384,
    TLS_IV   = binary:copy(<<(Seed + 0)>>, 8),
    TLS_KEY  = binary:copy(<<(Seed + 1)>>, 32),
    TLS_SALT = binary:copy(<<(Seed + 2)>>, 4),
    KtlsInfo =
        #{ socket => Socket,
           tls_version => TLS_version,
           cipher_suite => TLS_cipher,
           setopt_fun => fun inet_tls_dist:inet_ktls_setopt/3,
           getopt_fun => fun inet_tls_dist:inet_ktls_getopt/3 },
    CipherState =
        #cipher_state{
           key = TLS_KEY,
           iv = <<TLS_SALT/binary, TLS_IV/binary>> },
    inet_tls_dist:set_ktls_cipher(KtlsInfo, OS, CipherState, 0, TxRx).

ct_pal_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            ?CT_LOG("~s ~pB~n~s",
                    [FilePath, filelib:file_size(FilePath), Binary]);
        _ ->
            ?CT_LOG("Failed to log ~s", [FilePath]),
            ok
    end.
