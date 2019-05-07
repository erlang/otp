%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%

-module(ssl_eqc_cipher_format).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.

-define('TLS_v1.3', 'tlsv1.3').
-define('TLS_v1.2', 'tlsv1.2').
-define('TLS_v1.1', 'tlsv1.1').
-define('TLS_v1',   'tlsv1').
-define('SSL_v3',   'sslv3').

%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

prop_tls_cipher_suite_rfc_name() ->
    ?FORALL({CipherSuite, TLSVersion}, ?LET(Version, tls_version(), {cipher_suite(Version), Version}),
            case ssl:str_to_suite(ssl:suite_to_str(CipherSuite)) of
		CipherSuite ->
		    true;
		_ ->
		    false
	    end
	   ).

prop_tls_cipher_suite_openssl_name() ->
    ?FORALL({CipherSuite, TLSVersion}, ?LET(Version, tls_version(), {cipher_suite(Version), Version}),
            case ssl:str_to_suite(ssl:suite_to_openssl_str(CipherSuite)) of
		CipherSuite ->
		    true;
		_ ->
		    false
	    end
	   ).


%%--------------------------------------------------------------------
%% Generators  -----------------------------------------------
%%--------------------------------------------------------------------
tls_version() ->
    oneof([?'TLS_v1.2', ?'TLS_v1.1', ?'TLS_v1', ?'SSL_v3']).

cipher_suite(Version) ->
    oneof(cipher_suites(Version)).

cipher_suites(Version) ->
    ssl:cipher_suites(all, Version).

