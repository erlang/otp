%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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

-module(ssl_crl_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(TIMEOUT, 120000).
-define(LONG_TIMEOUT, 600000).
-define(SLEEP, 1000).
-define(OPENSSL_RENEGOTIATE, "R\n").
-define(OPENSSL_QUIT, "Q\n").
-define(OPENSSL_GARBAGE, "P\n").
-define(EXPIRE, 10).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [
     {group, basic},
     {group, v1_crl},
     {group, idp_crl}
    ].

groups() ->
    [{basic,   [], basic_tests()},
     {v1_crl,  [], v1_crl_tests()},
     {idp_crl, [], idp_crl_tests()}].

basic_tests() ->
    [crl_verify_valid, crl_verify_revoked].

v1_crl_tests() ->
    [crl_verify_valid, crl_verify_revoked].

idp_crl_tests() ->
    [crl_verify_valid, crl_verify_revoked].

%%%================================================================
%%% Suite init/end

init_per_suite(Config0) ->
    Dog = ct:timetrap(?LONG_TIMEOUT *2),
    case os:find_executable("openssl") of
	false ->
	    {skip, "Openssl not found"};
	_ ->
	    TLSVersion = ?config(tls_version, Config0),
	    OpenSSL_version = (catch os:cmd("openssl version")),
	    ct:log("TLS version: ~p~nOpenSSL version: ~p~n~n~p:module_info(): ~p~n~nssl:module_info(): ~p~n",
		   [TLSVersion, OpenSSL_version, ?MODULE, ?MODULE:module_info(), ssl:module_info()]),
	    case ssl_test_lib:enough_openssl_crl_support(OpenSSL_version) of
		false ->
		    {skip, io_lib:format("Bad openssl version: ~p",[OpenSSL_version])};
		_ ->
		    catch crypto:stop(),
		    try crypto:start() of
			ok ->
			    ssl:start(),
			    {ok, Hostname0} = inet:gethostname(),
			    IPfamily =
				case lists:member(list_to_atom(Hostname0), ct:get_config(ipv6_hosts,[])) of
				    true -> inet6;
				    false -> inet
				end,
			    [{ipfamily,IPfamily}, {watchdog, Dog}, {openssl_version,OpenSSL_version} | Config0]
		    catch _C:_E ->
			    ct:log("crypto:start() caught ~p:~p",[_C,_E]),
			    {skip, "Crypto did not start"}
		    end
	    end
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%%================================================================
%%% Group init/end

init_per_group(Group, Config) ->
    ssl:start(),
    inets:start(),
    CertDir = filename:join(?config(priv_dir, Config), Group),
    DataDir = ?config(data_dir, Config),
    ServerRoot = make_dir_path([?config(priv_dir,Config), Group, tmp]),
    %% start a HTTP server to serve the CRLs
    {ok, Httpd} = inets:start(httpd, [{ipfamily, ?config(ipfamily,Config)},
				      {server_name, "localhost"}, {port, 0},
				      {server_root, ServerRoot},
				      {document_root, CertDir},
				      {modules, [mod_get]}
				     ]),
    [{port,Port}] = httpd:info(Httpd, [port]),
    ct:log("~p:~p~nHTTPD IP family=~p, port=~p~n", [?MODULE, ?LINE, ?config(ipfamily,Config), Port]),
    CertOpts =  [{crl_port,Port}|cert_opts(Group)],
    Result =  make_certs:all(DataDir, CertDir, CertOpts),
    ct:log("~p:~p~nmake_certs:all(~n DataDir=~p,~n CertDir=~p,~n ServerRoot=~p~n Opts=~p~n) returned ~p~n", [?MODULE,?LINE,DataDir, CertDir, ServerRoot, CertOpts, Result]),
    [{make_cert_result, Result}, {cert_dir, CertDir}, {httpd, Httpd} | Config].

cert_opts(v1_crl)  -> [{v2_crls, false}];
cert_opts(idp_crl) -> [{issuing_distribution_point, true}];
cert_opts(_) -> [].

make_dir_path(PathComponents) ->
    lists:foldl(fun(F,P0) -> file:make_dir(P=filename:join(P0,F)), P end,
		"",
		PathComponents).


end_per_group(_GroupName, Config) ->
    case ?config(httpd, Config) of
	undefined -> ok;
	Pid -> 
	    ct:log("Stop httpd ~p",[Pid]),
	   ok = inets:stop(httpd, Pid)
		,ct:log("Stopped",[])
    end,
    inets:stop(),
    Config.

%%%================================================================
%%% Test cases

crl_verify_valid() ->
    [{doc,"Verify a simple valid CRL chain"}].
crl_verify_valid(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    PrivDir = ?config(cert_dir, Config),
    ServerOpts = [{keyfile, filename:join([PrivDir, "server", "key.pem"])},
		  {certfile, filename:join([PrivDir, "server", "cert.pem"])},
		  {cacertfile, filename:join([PrivDir, "server", "cacerts.pem"])}],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
			   %{mfa, {ssl_test_lib, no_result, []}},
			   {options, ServerOpts}]),
    ct:log("~p:~p~nreturn from ssl_test_lib:start_server:~n~p",[?MODULE,?LINE,Server]),
    Port = ssl_test_lib:inet_port(Server),

    CACerts = load_cert(filename:join([PrivDir, "erlangCA", "cacerts.pem"])),

    ClientOpts = [{cacerts, CACerts},
		  {verify, verify_peer},
		  {verify_fun, {fun validate_function/3, {CACerts, []}}}],


    ct:log("~p:~p~ncalling ssl_test_lib:start_client",[?MODULE,?LINE]),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_send, [Data]}},
					%{mfa, {ssl_test_lib, no_result, []}},
					{options, ClientOpts}]),
    ct:log("~p:~p~nreturn from ssl_test_lib:start_client:~n~p",[?MODULE,?LINE,Client]),

    ssl_test_lib:check_result(Client, ok,  Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

crl_verify_revoked() ->
    [{doc,"Verify a simple valid CRL chain"}].
crl_verify_revoked(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    PrivDir = ?config(cert_dir, Config),
    ServerOpts = [{keyfile, filename:join([PrivDir, "revoked", "key.pem"])},
		  {certfile, filename:join([PrivDir, "revoked", "cert.pem"])},
		  {cacertfile, filename:join([PrivDir, "revoked", "cacerts.pem"])}],
    ct:log("~p:~p~nserver opts ~p~n", [?MODULE,?LINE, ServerOpts]),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   %{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    CACerts = load_cert(filename:join([PrivDir, "erlangCA", "cacerts.pem"])),
    ClientOpts = [{cacerts, CACerts},
		  {verify, verify_peer},
		  {verify_fun, {fun validate_function/3, {CACerts, []}}}],

    {connect_failed, _} = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					%{mfa, {?MODULE, 
					       %erlang_ssl_receive, [Data]}},
					{mfa, {ssl_test_lib, no_result, []}},
					{options, ClientOpts}]),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    process_flag(trap_exit, false).

%%%================================================================
%%% Lib

erlang_ssl_receive(Socket, Data) ->
    ct:log("~p:~p~nConnection info: ~p~n",
		       [?MODULE,?LINE, ssl:connection_info(Socket)]),
    receive
	{ssl, Socket, Data} ->
	    ct:log("~p:~p~nReceived ~p~n",[?MODULE,?LINE, Data]),
	    %% open_ssl server sometimes hangs waiting in blocking read
	    ssl:send(Socket, "Got it"), 
	    ok;
	{ssl, Socket, Byte} when length(Byte) == 1 ->
	    erlang_ssl_receive(Socket, tl(Data));
	{Port, {data,Debug}} when is_port(Port) ->
	    ct:log("~p:~p~nopenssl ~s~n",[?MODULE,?LINE, Debug]),
	    erlang_ssl_receive(Socket,Data);
	Other ->
	    ct:fail({unexpected_message, Other})
    after 4000 ->
	    ct:fail({did_not_get, Data})
    end.


erlang_ssl_send(Socket, Data) ->
    ct:log("~p:~p~nConnection info: ~p~n",
		       [?MODULE,?LINE, ssl:connection_info(Socket)]),
    ssl:send(Socket, Data),
    ok.

load_certs(undefined) ->
    undefined;
load_certs(CertDir) ->
    case file:list_dir(CertDir) of
        {ok, Certs} ->
            load_certs(lists:map(fun(Cert) -> filename:join(CertDir, Cert)
                    end, Certs), []);
        {error, _} ->
            undefined
    end.

load_certs([], Acc) ->
    ct:log("~p:~p~nSuccessfully loaded ~p CA certificates~n", [?MODULE,?LINE, length(Acc)]),
    Acc;
load_certs([Cert|Certs], Acc) ->
    case filelib:is_dir(Cert) of
        true ->
            load_certs(Certs, Acc);
        _ ->
            %ct:log("~p:~p~nLoading certificate ~p~n", [?MODULE,?LINE, Cert]),
            load_certs(Certs, load_cert(Cert) ++ Acc)
    end.

load_cert(Cert) ->
    {ok, Bin} = file:read_file(Cert),
    case filename:extension(Cert) of
        ".der" ->
            %% no decoding necessary
            [Bin];
        _ ->
            %% assume PEM otherwise
            Contents = public_key:pem_decode(Bin),
            [DER || {Type, DER, Cipher} <- Contents, Type == 'Certificate', Cipher == 'not_encrypted']
    end.

%% @doc Validator function for SSL negotiation.
%%
validate_function(Cert, valid_peer, State) ->
    ct:log("~p:~p~nvaliding peer ~p with ~p intermediate certs~n",
                [?MODULE,?LINE, get_common_name(Cert), 
                 length(element(2, State))]),
    %% peer certificate validated, now check the CRL
    Res = (catch check_crl(Cert, State)),
    ct:log("~p:~p~nCRL validate result for ~p: ~p~n",
                [?MODULE,?LINE, get_common_name(Cert), Res]),
    {Res, State};
validate_function(Cert, valid, {TrustedCAs, IntermediateCerts}=State) ->
    case public_key:pkix_is_self_signed(Cert) of
        true ->
            ct:log("~p:~p~nroot certificate~n",[?MODULE,?LINE]),
            %% this is a root cert, no CRL
            {valid, {TrustedCAs, [Cert|IntermediateCerts]}};
        false ->
            %% check is valid CA certificate, add to the list of
            %% intermediates
            Res = (catch check_crl(Cert, State)),
            ct:log("~p:~p~nCRL intermediate CA validate result for ~p: ~p~n",
                        [?MODULE,?LINE, get_common_name(Cert), Res]),
            {Res, {TrustedCAs, [Cert|IntermediateCerts]}}
    end;
validate_function(_Cert, _Event, State) ->
    %ct:log("~p:~p~nignoring event ~p~n", [?MODULE,?LINE, _Event]),
    {valid, State}.

%% @doc Given a certificate, find CRL distribution points for the given
%%      certificate, fetch, and attempt to validate each CRL through
%%      issuer_function/4.
%%
check_crl(Cert, State) ->
    %% pull the CRL distribution point(s) out of the certificate, if any
    ct:log("~p:~p~ncheck_crl(~n Cert=~p,~nState=~p~n)",[?MODULE,?LINE,Cert,State]),
    case pubkey_cert:select_extension(
	   ?'id-ce-cRLDistributionPoints',
	   pubkey_cert:extensions_list(Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.extensions)) of
        undefined ->
            ct:log("~p:~p~nno CRL distribution points for ~p~n",
                         [?MODULE,?LINE, get_common_name(Cert)]),
            %% fail; we can't validate if there's no CRL
            no_crl;
        CRLExtension ->
	    ct:log("~p:~p~nCRLExtension=~p)",[?MODULE,?LINE,CRLExtension]),
            CRLDistPoints = CRLExtension#'Extension'.extnValue,
            DPointsAndCRLs = lists:foldl(fun(Point, Acc) -> 
                            %% try to read the CRL over http or from a
                            %% local file
                            case fetch_point(Point) of
                                not_available ->
				    ct:log("~p:~p~nfetch_point returned~n~p~n)",[?MODULE,?LINE,not_available]),
                                    Acc;
                                Res ->
				    ct:log("~p:~p~nfetch_point returned~n~p~n)",[?MODULE,?LINE,Res]),
                                    [{Point, Res} | Acc]
                            end 
                    end, [], CRLDistPoints),
            public_key:pkix_crls_validate(Cert, 
                                          DPointsAndCRLs, 
                                          [{issuer_fun, 
                                            {fun issuer_function/4, State}}])
    end.

%% @doc Given a list of distribution points for CRLs, certificates and
%%      both trusted and intermediary certificates, attempt to build and
%%      authority chain back via build_chain to verify that it is valid.
%%
issuer_function(_DP, CRL, _Issuer, {TrustedCAs, IntermediateCerts}) ->
    %% XXX the 'Issuer' we get passed here is the AuthorityKeyIdentifier,
    %% which we are not currently smart enough to understand
    %% Read the CA certs out of the file
    ct:log("~p:~p~nissuer_function(~nCRL=~p,~nLast param=~p)",[?MODULE,?LINE,CRL, {TrustedCAs, IntermediateCerts}]),
    Certs = [public_key:pkix_decode_cert(DER, otp) || DER <- TrustedCAs],
    %% get the real issuer out of the CRL
    Issuer = public_key:pkix_normalize_name(
            pubkey_cert_records:transform(
                CRL#'CertificateList'.tbsCertList#'TBSCertList'.issuer, decode)),
    %% assume certificates are ordered from root to tip
    case find_issuer(Issuer, IntermediateCerts ++ Certs) of
        undefined ->
            ct:log("~p:~p~nunable to find certificate matching CRL issuer ~p~n", 
                        [?MODULE,?LINE, Issuer]),
            error;
        IssuerCert ->
	    ct:log("~p:~p~nIssuerCert=~p~n)",[?MODULE,?LINE,IssuerCert]),
            case build_chain({public_key:pkix_encode('OTPCertificate', 
                                                     IssuerCert, 
                                                     otp), 
                              IssuerCert}, IntermediateCerts, Certs, []) of
                undefined ->
                    error;
                {OTPCert, Path} ->
                    {ok, OTPCert, Path}
            end
    end.

%% @doc Attempt to build authority chain back using intermediary
%%      certificates, falling back on trusted certificates if the
%%      intermediary chain of certificates does not fully extend to the 
%%      root.
%% 
%%      Returns: {RootCA :: #OTPCertificate{}, Chain :: [der_encoded()]}
%%
build_chain({DER, Cert}, IntCerts, TrustedCerts, Acc) ->
    %% check if this cert is self-signed, if it is, we've reached the
    %% root of the chain
    Issuer = public_key:pkix_normalize_name(
            Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer),
    Subject = public_key:pkix_normalize_name(
            Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject),
    case Issuer == Subject of
        true ->
            case find_issuer(Issuer, TrustedCerts) of
                undefined ->
                    ct:log("~p:~p~nself-signed certificate is NOT trusted~n",[?MODULE,?LINE]),
                    undefined;
                TrustedCert ->
                    %% return the cert from the trusted list, to prevent
                    %% issuer spoofing
                    {TrustedCert, 
                     [public_key:pkix_encode(
                                'OTPCertificate', TrustedCert, otp)|Acc]}
            end;
        false ->
            Match = lists:foldl(
                      fun(C, undefined) ->
                              S = public_key:pkix_normalize_name(C#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject),
                              %% compare the subject to the current issuer
                              case Issuer == S of
                                  true ->
                                      %% we've found our man
                                      {public_key:pkix_encode('OTPCertificate', C, otp), C};
                                  false ->
                                      undefined
                              end;
                         (_E, A) ->
                              %% already matched
                              A
                      end, undefined, IntCerts),
            case Match of
                undefined when IntCerts /= TrustedCerts ->
                    %% continue the chain by using the trusted CAs
                    ct:log("~p:~p~nRan out of intermediate certs, switching to trusted certs~n",[?MODULE,?LINE]),
                    build_chain({DER, Cert}, TrustedCerts, TrustedCerts, Acc);
                undefined ->
                    ct:log("Can't construct chain of trust beyond ~p~n",
                                [?MODULE,?LINE, get_common_name(Cert)]),
                    %% can't find the current cert's issuer
                    undefined;
                Match ->
                    build_chain(Match, IntCerts, TrustedCerts, [DER|Acc])
            end
    end.

%% @doc Given a certificate and a list of trusted or intermediary
%%      certificates, attempt to find a match in the list or bail with
%%      undefined.
find_issuer(Issuer, Certs) ->
    lists:foldl(
      fun(OTPCert, undefined) ->
              %% check if this certificate matches the issuer
              Normal = public_key:pkix_normalize_name(
                        OTPCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject),
              case Normal == Issuer of
                  true ->
                      OTPCert;
                  false ->
                      undefined
              end;
         (_E, Acc) ->
              %% already found a match
              Acc
      end, undefined, Certs).

%% @doc Find distribution points for a given CRL and then attempt to
%%      fetch the CRL from the first available.
fetch_point(#'DistributionPoint'{distributionPoint={fullName, Names}}) ->
    Decoded = [{NameType, 
                pubkey_cert_records:transform(Name, decode)} 
               || {NameType, Name} <- Names],
    ct:log("~p:~p~ncall fetch(~nDecoded=~p~n)",[?MODULE,?LINE,Decoded]),
    fetch(Decoded).

%% @doc Given a list of locations to retrieve a CRL from, attempt to
%%      retrieve either from a file or http resource and bail as soon as
%%      it can be found.
%%
%%      Currently, only hand a armored PEM or DER encoded file, with
%%      defaulting to DER.
%%
fetch([]) ->
    not_available;
fetch([{uniformResourceIdentifier, "http"++_=URL}|Rest]) ->
    ct:log("~p:~p~ngetting CRL from ~p~n", [?MODULE,?LINE, URL]),
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} ->
            case Body of
                <<"-----BEGIN", _/binary>> ->
		    ct:log("~p:~p~npublic_key:pem_decode,~nBody=~p~n)",[?MODULE,?LINE,Body]),
                    [{'CertificateList', 
                      DER, _}=CertList] = public_key:pem_decode(Body),
		    ct:log("~p:~p~npublic_key:pem_entry_decode,~nCertList=~p~n)",[?MODULE,?LINE,CertList]),
                    {DER, public_key:pem_entry_decode(CertList)};
                _ ->
		    ct:log("~p:~p~npublic_key:pem_entry_decode,~nBody=~p~n)",[?MODULE,?LINE,{'CertificateList', Body, not_encrypted}]),
                    %% assume DER encoded
		    try 
			public_key:pem_entry_decode({'CertificateList', Body, not_encrypted})
		    of 
			CertList -> {Body, CertList}
		    catch
			_C:_E ->
			    ct:log("~p:~p~nfailed DER assumption~nRest=~p", [?MODULE,?LINE,Rest]),
			    fetch(Rest)
		    end
            end;
        {error, _Reason} ->
            ct:log("~p:~p~nfailed to get CRL ~p~n", [?MODULE,?LINE, _Reason]),
            fetch(Rest);
	Other ->
            ct:log("~p:~p~nreally failed to get CRL ~p~n", [?MODULE,?LINE, Other]),
            fetch(Rest)
    end;
fetch([Loc|Rest]) ->
    %% unsupported CRL location
    ct:log("~p:~p~nunable to fetch CRL from unsupported location ~p~n", 
                [?MODULE,?LINE, Loc]),
    fetch(Rest).

%% get the common name attribute out of an OTPCertificate record
get_common_name(OTPCert) ->
    %% You'd think there'd be an easier way than this giant mess, but I
    %% couldn't find one.
    {rdnSequence, Subject} = OTPCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
    case [Attribute#'AttributeTypeAndValue'.value || [Attribute] <- Subject,
        Attribute#'AttributeTypeAndValue'.type == ?'id-at-commonName'] of
        [Att] ->
            case Att of
                {teletexString, Str} -> Str;
                {printableString, Str} -> Str;
                {utf8String, Bin} -> binary_to_list(Bin)
            end;
        _ ->
            unknown
    end.

