%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026-2026. All Rights Reserved.
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

-module(ssl_crl_white_box_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Common test
-export([all/0]).

-export([crl_fetch_internal_ip_blocked/0,
         crl_fetch_internal_ip_blocked/1,
         crl_fetch_allowed_hosts/0,
         crl_fetch_allowed_hosts/1,
         crl_fetch_oversized_rejected/0,
         crl_fetch_oversized_rejected/1,
         crl_malformed_der_ignored/0,
         crl_malformed_der_ignored/1,
         crl_fetch_non_http_scheme_blocked/0,
         crl_fetch_non_http_scheme_blocked/1]).

all() ->
    [crl_fetch_internal_ip_blocked,
     crl_fetch_allowed_hosts,
     crl_fetch_oversized_rejected,
     crl_malformed_der_ignored,
     crl_fetch_non_http_scheme_blocked
    ].

%%--------------------------------------------------------------------
%% CRL Cache White Box Tests
%%--------------------------------------------------------------------
crl_fetch_internal_ip_blocked() ->
    [{doc, "Verify that CRL fetch to internal/loopback IPs is blocked (SSRF protection)"}].
crl_fetch_internal_ip_blocked(_Config) ->
    %% Test the internal IP detection directly
    true = ssl_crl_cache:is_internal_ip({127, 0, 0, 1}),
    true = ssl_crl_cache:is_internal_ip({10, 0, 0, 1}),
    true = ssl_crl_cache:is_internal_ip({172, 16, 0, 1}),
    true = ssl_crl_cache:is_internal_ip({172, 31, 255, 255}),
    true = ssl_crl_cache:is_internal_ip({192, 168, 1, 1}),
    true = ssl_crl_cache:is_internal_ip({169, 254, 0, 1}),
    true = ssl_crl_cache:is_internal_ip({0, 0, 0, 0}),
    %% IPv6
    true = ssl_crl_cache:is_internal_ip({0,0,0,0,0,0,0,1}),
    true = ssl_crl_cache:is_internal_ip({0,0,0,0,0,0,0,0}),
    true = ssl_crl_cache:is_internal_ip({16#fe80,0,0,0,0,0,0,1}),
    true = ssl_crl_cache:is_internal_ip({16#fc00,0,0,0,0,0,0,1}),
    true = ssl_crl_cache:is_internal_ip({16#fdff,0,0,0,0,0,0,1}),
    %% External IPs should pass
    false = ssl_crl_cache:is_internal_ip({8, 8, 8, 8}),
    false = ssl_crl_cache:is_internal_ip({172, 15, 0, 1}),
    false = ssl_crl_cache:is_internal_ip({172, 32, 0, 1}),
    false = ssl_crl_cache:is_internal_ip({192, 167, 1, 1}),
    false = ssl_crl_cache:is_internal_ip({1, 2, 3, 4}),
    %% Test via valid_host with a URL resolving to loopback
    URI = uri_string:normalize("http://localhost/crl.pem", [return_map]),
    {disallowed, _} = ssl_crl_cache:validate_host(URI, [], {127,0,0,1},inet),
    ok.

crl_fetch_allowed_hosts() ->
    [{doc, "Verify that allowed_hosts allowlist permits configured hosts "
     "and blocks others"}].
crl_fetch_allowed_hosts(_Config) ->
    %% is_allowed with matching host and default port
    true = ssl_crl_cache:is_allowed("crl.example.com", 80,
                                    ["crl.example.com"]),
    false = ssl_crl_cache:is_allowed("crl.example.com", 8080,
                                    ["crl.example.com"]),
    %% is_allowed with explicit port
    true = ssl_crl_cache:is_allowed("crl.example.com", 8080,
                                    ["crl.example.com:8080"]),
    %% Not in allowlist
    false = ssl_crl_cache:is_allowed("evil.com", 80,
                                     ["crl.example.com"]),
    %% Port mismatch (non-default port, no explicit port in allowlist)
    false = ssl_crl_cache:is_allowed("crl.example.com", 9090,
                                     ["crl.example.com", "crl.example.com:8080"]),
    %% Empty allowlist — always false (falls through to external IP check)
    false = ssl_crl_cache:is_allowed("anything.com", 80, []),
    URI0 = uri_string:normalize("http://crl.example.com/crl.pem", [return_map]),
    URI1 = uri_string:normalize("http://crl.example.com:8080/crl.pem", [return_map]),
    URI2 = uri_string:normalize("http://crl.example.com:9090/crl.pem", [return_map]),
    {ok, IP} = inet:getaddr("www.example.com", inet),
    ok = ssl_crl_cache:validate_host(URI0, [], IP, inet),
    ok = ssl_crl_cache:validate_host(URI1, [], IP, inet),
    {disallowed, not_standard_port} = ssl_crl_cache:validate_host(URI2, [], IP, inet),
    ok.

crl_fetch_oversized_rejected() ->
    [{doc, "Verify that CRL responses exceeding MAX_CRL_SIZE are rejected"}].
crl_fetch_oversized_rejected(Config) ->
    %% Start a local HTTP server that serves an oversized response
    PrivDir = proplists:get_value(priv_dir, Config),
    ServerRoot = filename:join(PrivDir, "oversized_crl_server"),
    DocRoot = filename:join(ServerRoot, "docs"),
    ok = filelib:ensure_dir(filename:join(DocRoot, "dummy")),
    %% Create a file larger than 10MB
    OversizedFile = filename:join(DocRoot, "huge.crl"),
    OversizedData = binary:copy(<<0>>, 10 * 1024 * 1024 + 1),
    ok = file:write_file(OversizedFile, OversizedData),
    application:ensure_started(inets),
    {ok, Httpd} = inets:start(httpd, [{server_name, "localhost"},
                                      {port, 0},
                                      {server_root, ServerRoot},
                                      {document_root, DocRoot},
                                      {ipfamily, inet}]),
    [{port, Port}] = httpd:info(Httpd, [port]),
    URL = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/huge.crl",
    %% Use a temporary ets table as a valid cache to avoid badarg
    %% on cache lookup.
    PortStr = integer_to_list(Port),
    Cache = ets:new(test_crl_cache, [set, public]),
    Mapping = ets:new(test_crl_mapping, [set, public]),
    CRLDbInfo = {{Cache, Mapping},
                 [{http, 5000}, {owner, normal},
                  {allowed_hosts, ["127.0.0.1:" ++ PortStr]}]},
    DP = #'DistributionPoint'{
            distributionPoint =
                {fullName, [{uniformResourceIdentifier, URL}]}},
    Result = ssl_crl_cache:lookup(DP, undefined, CRLDbInfo),
    %% Should not return a CRL (oversized body rejected)
    not_available = Result,
    ets:delete(Cache),
    ets:delete(Mapping),
    inets:stop(httpd, Httpd),
    ok.

crl_malformed_der_ignored() ->
    [{doc, "Verify that malformed CRL DER data does not crash ssl_manager"}].
crl_malformed_der_ignored(_Config) ->
    %% Insert a malformed CRL via the documented API — previously this
    %% would crash ssl_manager via unguarded public_key:der_decode
    ssl_test_lib:clean_start(),
    MalformedDER = <<"not a valid DER CertificateList">>,
    %% This should not crash ssl_manager
    ok = ssl_crl_cache:insert({der, [MalformedDER]}),
    %% Verify ssl_manager is still alive
    true = is_pid(whereis(ssl_manager)),
    %% Also test via URI insertion
    ok = ssl_crl_cache:insert("http://example.com/bad.crl",
                              {der, [MalformedDER]}),
    true = is_pid(whereis(ssl_manager)),
    ok.

crl_fetch_non_http_scheme_blocked() ->
    [{doc, "Verify that non-HTTP schemes (ftp, file, https) in CRL DPs "
     "are silently skipped — only http:// triggers a fetch"}].
crl_fetch_non_http_scheme_blocked(_Config) ->
    %% CRL Distribution Points with non-http schemes should result
    %% in not_available — get_crls only matches "http"++_ URLs.
    %% Use undefined as CRLDbInfo to avoid needing a live cache for
    %% scheme filtering (cache_lookup(_, undefined) -> []).
    CRLDbInfo = undefined,
    %% ftp scheme
    DP_ftp = #'DistributionPoint'{
               distributionPoint =
                   {fullName, [{uniformResourceIdentifier,
                                "ftp://evil.com/crl.pem"}]}},
    not_available = ssl_crl_cache:lookup(DP_ftp, undefined, CRLDbInfo),
    %% file scheme
    DP_file = #'DistributionPoint'{
                distributionPoint =
                    {fullName, [{uniformResourceIdentifier,
                                 "file:///etc/passwd"}]}},
    not_available = ssl_crl_cache:lookup(DP_file, undefined, CRLDbInfo),
    %% https scheme (would cause recursive TLS, not supported)
    DP_https = #'DistributionPoint'{
                 distributionPoint =
                     {fullName, [{uniformResourceIdentifier,
                                  "https://ca.example.com/crl"}]}},
    not_available = ssl_crl_cache:lookup(DP_https, undefined, CRLDbInfo),
    ok.
