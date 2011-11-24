%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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
%%----------------------------------------------------------------------
-module(ssh_test_lib).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("public_key/include/public_key.hrl").
-include("test_server.hrl").
-include("test_server_line.hrl").

-define(TIMEOUT, 50000).

connect(Options) ->
    connect(hostname(), inet_port(), Options).

connect(Port, Options) when is_integer(Port) ->
    connect(hostname(), Port, Options);
connect(any, Options) ->
    connect(hostname(), inet_port(), Options);
connect(Host, Options) ->
    connect(Host, inet_port(), Options).

connect(any, Port, Options) ->
    connect(hostname(), Port, Options);
connect(Host, Port, Options) ->
    case ssh:connect(Host, Port, Options) of
	{ok, ConnectionRef} ->
	    ConnectionRef;
	Error ->
	    Error
    end.

daemon(Options) ->
    daemon(any, inet_port(), Options).

daemon(Port, Options) when is_integer(Port) ->
    daemon(any, Port, Options);
daemon(Host, Options) ->
    daemon(Host, inet_port(), Options).

daemon(Host, Port, Options) ->
    case ssh:daemon(Host, Port, Options) of
	{ok, Pid} when Host == any ->
	    {Pid, hostname(), Port};
	{ok, Pid} ->
	    {Pid, Host, Port};
	Error ->
	    Error
    end.




start_shell(Port, IOServer) ->
    spawn_link(?MODULE, init_shell, [Port, IOServer]).

init_shell(Port, IOServer) ->
    Host = hostname(),
    UserDir = get_user_dir(),
    Options = [{user_interaction, false}, {silently_accept_hosts,
					   true}] ++ UserDir,
    group_leader(IOServer, self()),
    loop_shell(Host, Port, Options).

loop_shell(Host, Port, Options) ->
    ssh:shell(Host, Port, Options).

start_io_server() ->
    spawn_link(?MODULE, init_io_server, [self()]).

init_io_server(TestCase) ->
    process_flag(trap_exit, true),
    loop_io_server(TestCase, []).

loop_io_server(TestCase, Buff0) ->
     receive
	 {input, TestCase, Line} ->
	     %io:format("~p~n",[{input, TestCase, Line}]),
	     loop_io_server(TestCase, Buff0 ++ [Line]);
	 {io_request, From, ReplyAs, Request} ->
	     %io:format("request -> ~p~n",[Request]),
	     {ok, Reply, Buff} = io_request(Request, TestCase, From,
					    ReplyAs, Buff0),
	     %io:format("reply -> ~p~n",[Reply]),
	     io_reply(From, ReplyAs, Reply),
	     loop_io_server(TestCase, Buff);
	 {'EXIT',_, _} ->
	     erlang:display('EXIT'),
	     ok
     end.

io_request({put_chars, Chars}, TestCase, _, _, Buff) ->
    reply(TestCase, Chars),
    {ok, ok, Buff};
io_request({put_chars, Enc, Chars}, TestCase, _, _, Buff) ->
    reply(TestCase, unicode:characters_to_binary(Chars,Enc,latin1)),
    {ok, ok, Buff};

io_request({get_line, _} = Request, _, From, ReplyAs, [] = Buff) ->
    erlang:send_after(1000, self(), {io_request, From, ReplyAs, Request}),
    {ok, [], Buff};
io_request({get_line, _Enc, _Prompt} = Request, _, From, ReplyAs, [] = Buff) ->
    erlang:send_after(1000, self(), {io_request, From, ReplyAs, Request}),
    {ok, [], Buff};

io_request({get_line, _Enc,_}, _, _, _, [Line | Buff]) ->
    {ok, Line, Buff}.

io_reply(_, _, []) ->
    ok;
io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

reply(_, []) ->
    ok;
reply(TestCase, Result) ->
    TestCase ! Result.

receive_exec_result(Msg) ->
    test_server:format("Expect data! ~p", [Msg]),
    receive
	Msg ->
	    test_server:format("1: Collected data ~p", [Msg]),
	    expected;
	Other ->
	    {unexpected_msg, Other}
    end.
receive_exec_end(ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    ExitStatus = {ssh_cm, ConnectionRef, {exit_status, ChannelId, 0}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    case receive_exec_result(ExitStatus) of
	{unexpected_msg, Eof} -> %% Open ssh seems to not allways send these messages
	    %% in the same order!
	    test_server:format("2: Collected data ~p", [Eof]),
	    case receive_exec_result(ExitStatus) of
		expected ->
		    expected = receive_exec_result(Closed);
		{unexpected_msg, Closed} ->
		    test_server:format("3: Collected data ~p", [Closed])
	    end;
	expected ->
	    test_server:format("4: Collected data ~p", [ExitStatus]),
	    expected = receive_exec_result(Eof),
	    expected = receive_exec_result(Closed);
	Other ->
	    test_server:fail({unexpected_msg, Other})
    end.

receive_exec_result(Data, ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    expected = receive_exec_result(Data),
    expected = receive_exec_result(Eof),
    expected = receive_exec_result(Closed).


inet_port()->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    gen_tcp:close(Socket),
    Port.


%% copy private keys to given dir from ~/.ssh
get_id_keys(DstDir) ->
    SrcDir = filename:join(os:getenv("HOME"), ".ssh"),
    RsaOk = copyfile(SrcDir, DstDir, "id_rsa"),
    DsaOk = copyfile(SrcDir, DstDir, "id_dsa"),
    case {RsaOk, DsaOk} of
	{{ok, _}, {ok, _}} -> {ok, both};
	{{ok, _}, _} -> {ok, rsa};
	{_, {ok, _}} -> {ok, dsa};
	{Error, _} -> Error
    end.

remove_id_keys(Dir) ->
    file:delete(filename:join(Dir, "id_rsa")),
    file:delete(filename:join(Dir, "id_dsa")).

copyfile(SrcDir, DstDir, Fn) ->
    file:copy(filename:join(SrcDir, Fn),
	      filename:join(DstDir, Fn)).

failfun(_User, {authmethod,none}) ->
    ok;
failfun(User, Reason) ->
    error_logger:format("~p failed XXX to login: ~p~n", [User, Reason]).

hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.

known_hosts(BR) ->
    KnownHosts = ssh_file:file_name(user, "known_hosts", []),
    B = KnownHosts ++ "xxx",
    case BR of
	backup ->
	    file:rename(KnownHosts, B);
	restore ->
	    file:delete(KnownHosts),
	    file:rename(B, KnownHosts)
    end.


get_user_dir() ->
    case os:type() of
	{win32, _} ->
	    [{user_dir, filename:join([os:getenv("HOME"), ".ssh"])}];
	_ ->
	    []
    end.


make_dsa_cert_files(Config) ->    
    make_dsa_cert_files("", Config).

make_dsa_cert_files(RoleStr, Config) ->    
    
    CaInfo = {CaCert, _} = make_cert([{key, dsa}]),
    {Cert, CertKey} = make_cert([{key, dsa}, {issuer, CaInfo}]),
    CaCertFile = filename:join([?config(data_dir, Config), 
 				RoleStr, "dsa_cacerts.pem"]),
    CertFile = filename:join([?config(data_dir, Config), 
 			      RoleStr, "dsa_cert.pem"]),
    KeyFile = filename:join([?config(data_dir, Config), 
			     RoleStr, "dsa_key.pem"]),
    
    der_to_pem(CaCertFile, [{'Certificate', CaCert, not_encrypted}]),
    der_to_pem(CertFile, [{'Certificate', Cert, not_encrypted}]),
    der_to_pem(KeyFile, [CertKey]),
    {CaCertFile, CertFile, KeyFile}.

make_dsa_files(Config) ->
    make_dsa_files(Config, rfc4716_public_key).
make_dsa_files(Config, Type) ->
    {DSA, EncodedKey} = ssh_test_lib:gen_dsa(128, 20),
    PKey = DSA#'DSAPrivateKey'.y,
    P = DSA#'DSAPrivateKey'.p,
    Q = DSA#'DSAPrivateKey'.q,
    G = DSA#'DSAPrivateKey'.g,
    Dss = #'Dss-Parms'{p=P, q=Q, g=G},
    {ok, Hostname} = inet:gethostname(),
    {ok, {A, B, C, D}} = inet:getaddr(Hostname, inet),
    IP = lists:concat([A, ".", B, ".", C, ".", D]),
    Attributes = [], % Could be [{comment,"user@" ++ Hostname}],
    HostNames = [{hostnames,[IP, IP]}],
    PublicKey = [{{PKey, Dss}, Attributes}],
    KnownHosts = [{{PKey, Dss}, HostNames}],

    KnownHostsEnc = public_key:ssh_encode(KnownHosts, known_hosts),
    KnownHosts = public_key:ssh_decode(KnownHostsEnc, known_hosts),

    PublicKeyEnc = public_key:ssh_encode(PublicKey, Type),
%    PublicKey = public_key:ssh_decode(PublicKeyEnc, Type),

    SystemTmpDir = ?config(data_dir, Config),
    filelib:ensure_dir(SystemTmpDir),
    file:make_dir(SystemTmpDir),
    
    DSAFile = filename:join(SystemTmpDir, "ssh_host_dsa_key.pub"),
    file:delete(DSAFile),
    
    DSAPrivateFile  = filename:join(SystemTmpDir, "ssh_host_dsa_key"),
    file:delete(DSAPrivateFile),

    KHFile = filename:join(SystemTmpDir, "known_hosts"),
    file:delete(KHFile),
    
    PemBin = public_key:pem_encode([EncodedKey]),

    file:write_file(DSAFile, PublicKeyEnc),
    file:write_file(KHFile, KnownHostsEnc),
    file:write_file(DSAPrivateFile, PemBin),
    ok.

%%--------------------------------------------------------------------
%% Create and return a der encoded certificate
%%   Option                                         Default
%%   -------------------------------------------------------
%%   digest                                         sha1
%%   validity                                       {date(), date() + week()}
%%   version                                        3
%%   subject                                        [] list of the following content
%%      {name,  Name}
%%      {email, Email} 
%%      {city,  City}
%%      {state, State}
%%      {org, Org}
%%      {org_unit, OrgUnit}
%%      {country, Country} 
%%      {serial, Serial}
%%      {title, Title}
%%      {dnQualifer, DnQ}
%%   issuer = {Issuer, IssuerKey}                   true (i.e. a ca cert is created) 
%%                                                  (obs IssuerKey migth be {Key, Password}
%%   key = KeyFile|KeyBin|rsa|dsa                   Subject PublicKey rsa or dsa generates key
%%   
%%
%%   (OBS: The generated keys are for testing only)
%% make_cert([{::atom(), ::term()}]) -> {Cert::binary(), Key::binary()}
%%--------------------------------------------------------------------
make_cert(Opts) ->
    SubjectPrivateKey = get_key(Opts),
    {TBSCert, IssuerKey} = make_tbs(SubjectPrivateKey, Opts),
    Cert = public_key:pkix_sign(TBSCert, IssuerKey),
    true = verify_signature(Cert, IssuerKey, undef), %% verify that the keys where ok
    {Cert, encode_key(SubjectPrivateKey)}.

%%--------------------------------------------------------------------
%% Writes cert files in Dir with FileName and FileName ++ Suffix
%% write_cert(::string(), ::string(), {Cert,Key}) -> ok
%%--------------------------------------------------------------------
write_cert(Dir, FileName, Suffix, {Cert, Key = {_,_,not_encrypted}}) when is_binary(Cert) ->
    ok = der_to_pem(filename:join(Dir, FileName),
			       [{'Certificate', Cert, not_encrypted}]),
    ok = der_to_pem(filename:join(Dir, FileName ++ Suffix), [Key]).

%%--------------------------------------------------------------------
%% Creates a rsa key (OBS: for testing only)
%%   the size are in bytes
%% gen_rsa(::integer()) -> {::atom(), ::binary(), ::opaque()}
%%--------------------------------------------------------------------
gen_rsa(Size) when is_integer(Size) ->
    Key = gen_rsa2(Size),
    {Key, encode_key(Key)}.

%%--------------------------------------------------------------------
%% Creates a dsa key (OBS: for testing only)
%%   the sizes are in bytes
%% gen_dsa(::integer()) -> {::atom(), ::binary(), ::opaque()}
%%--------------------------------------------------------------------
gen_dsa(LSize,NSize) when is_integer(LSize), is_integer(NSize) ->
    Key = gen_dsa2(LSize, NSize),
    {Key, encode_key(Key)}.

%%--------------------------------------------------------------------
%% Verifies cert signatures
%% verify_signature(::binary(), ::tuple()) -> ::boolean()
%%--------------------------------------------------------------------
verify_signature(DerEncodedCert, DerKey, _KeyParams) ->
    Key = decode_key(DerKey),
    case Key of 
	#'RSAPrivateKey'{modulus=Mod, publicExponent=Exp} ->
	    public_key:pkix_verify(DerEncodedCert, 
				   #'RSAPublicKey'{modulus=Mod, publicExponent=Exp});
	#'DSAPrivateKey'{p=P, q=Q, g=G, y=Y} ->
	    public_key:pkix_verify(DerEncodedCert, {Y, #'Dss-Parms'{p=P, q=Q, g=G}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%% Implementation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_key(Opts) ->
    case proplists:get_value(key, Opts) of
	undefined -> make_key(rsa, Opts);
	rsa ->       make_key(rsa, Opts);
	dsa ->       make_key(dsa, Opts);
	Key ->
	    Password = proplists:get_value(password, Opts, no_passwd),
	    decode_key(Key, Password)
    end.

decode_key({Key, Pw}) ->
    decode_key(Key, Pw);
decode_key(Key) ->
    decode_key(Key, no_passwd).
    

decode_key(#'RSAPublicKey'{} = Key,_) ->
    Key;
decode_key(#'RSAPrivateKey'{} = Key,_) ->
    Key;
decode_key(#'DSAPrivateKey'{} = Key,_) ->
    Key;
decode_key(PemEntry = {_,_,_}, Pw) ->
    public_key:pem_entry_decode(PemEntry, Pw);
decode_key(PemBin, Pw) ->
    [KeyInfo] = public_key:pem_decode(PemBin),
    decode_key(KeyInfo, Pw).

encode_key(Key = #'RSAPrivateKey'{}) ->
    {ok, Der} = 'OTP-PUB-KEY':encode('RSAPrivateKey', Key),
    {'RSAPrivateKey', list_to_binary(Der), not_encrypted};   
encode_key(Key = #'DSAPrivateKey'{}) ->
    {ok, Der} = 'OTP-PUB-KEY':encode('DSAPrivateKey', Key),
    {'DSAPrivateKey', list_to_binary(Der), not_encrypted}.

make_tbs(SubjectKey, Opts) ->    
    Version = list_to_atom("v"++integer_to_list(proplists:get_value(version, Opts, 3))),

    IssuerProp = proplists:get_value(issuer, Opts, true),
    {Issuer, IssuerKey}  = issuer(IssuerProp, Opts, SubjectKey),

    {Algo, Parameters} = sign_algorithm(IssuerKey, Opts),
    
    SignAlgo = #'SignatureAlgorithm'{algorithm  = Algo,
				     parameters = Parameters},    
    Subject = case IssuerProp of
		  true -> %% Is a Root Ca
		      Issuer;
		  _ ->
		      subject(proplists:get_value(subject, Opts),false)
	      end,

    {#'OTPTBSCertificate'{serialNumber = trunc(random:uniform()*100000000)*10000 + 1,
			  signature    = SignAlgo,
			  issuer       = Issuer,
			  validity     = validity(Opts),
			  subject      = Subject,
			  subjectPublicKeyInfo = publickey(SubjectKey),
			  version      = Version,
			  extensions   = extensions(Opts)
			 }, IssuerKey}.

issuer(true, Opts, SubjectKey) ->
    %% Self signed
    {subject(proplists:get_value(subject, Opts), true), SubjectKey};
issuer({Issuer, IssuerKey}, _Opts, _SubjectKey) when is_binary(Issuer) ->
    {issuer_der(Issuer), decode_key(IssuerKey)};
issuer({File, IssuerKey}, _Opts, _SubjectKey) when is_list(File) ->
    {ok, [{cert, Cert, _}|_]} = pem_to_der(File),
    {issuer_der(Cert), decode_key(IssuerKey)}.

issuer_der(Issuer) ->
    Decoded = public_key:pkix_decode_cert(Issuer, otp),
    #'OTPCertificate'{tbsCertificate=Tbs} = Decoded,
    #'OTPTBSCertificate'{subject=Subject} = Tbs,
    Subject.

subject(undefined, IsRootCA) ->
    User = if IsRootCA -> "RootCA"; true -> os:getenv("USER") end,
    Opts = [{email, User ++ "@erlang.org"},
	    {name, User},
	    {city, "Stockholm"},
	    {country, "SE"},
	    {org, "erlang"},
	    {org_unit, "testing dep"}],
    subject(Opts);
subject(Opts, _) ->
    subject(Opts).

subject(SubjectOpts) when is_list(SubjectOpts) ->
    Encode = fun(Opt) ->
		     {Type,Value} = subject_enc(Opt),
		     [#'AttributeTypeAndValue'{type=Type, value=Value}]
	     end,
    {rdnSequence, [Encode(Opt) || Opt <- SubjectOpts]}.

%% Fill in the blanks
subject_enc({name,  Name}) ->       {?'id-at-commonName', {printableString, Name}};
subject_enc({email, Email}) ->      {?'id-emailAddress', Email};
subject_enc({city,  City}) ->       {?'id-at-localityName', {printableString, City}};
subject_enc({state, State}) ->      {?'id-at-stateOrProvinceName', {printableString, State}};
subject_enc({org, Org}) ->          {?'id-at-organizationName', {printableString, Org}};
subject_enc({org_unit, OrgUnit}) -> {?'id-at-organizationalUnitName', {printableString, OrgUnit}};
subject_enc({country, Country}) ->  {?'id-at-countryName', Country};
subject_enc({serial, Serial}) ->    {?'id-at-serialNumber', Serial};
subject_enc({title, Title}) ->      {?'id-at-title', {printableString, Title}};
subject_enc({dnQualifer, DnQ}) ->   {?'id-at-dnQualifier', DnQ};
subject_enc(Other) ->               Other.


extensions(Opts) ->
    case proplists:get_value(extensions, Opts, []) of
	false -> 
	    asn1_NOVALUE;
	Exts  -> 
	    lists:flatten([extension(Ext) || Ext <- default_extensions(Exts)])
    end.

default_extensions(Exts) ->
    Def = [{key_usage,undefined}, 
	   {subject_altname, undefined},
	   {issuer_altname, undefined},
	   {basic_constraints, default},
	   {name_constraints, undefined},
	   {policy_constraints, undefined},
	   {ext_key_usage, undefined},
	   {inhibit_any, undefined},
	   {auth_key_id, undefined},
	   {subject_key_id, undefined},
	   {policy_mapping, undefined}],
    Filter = fun({Key, _}, D) -> lists:keydelete(Key, 1, D) end,
    Exts ++ lists:foldl(Filter, Def, Exts).
       	
extension({_, undefined}) -> [];
extension({basic_constraints, Data}) ->
    case Data of
	default ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = #'BasicConstraints'{cA=true},
			 critical=true};
	false -> 
	    [];
	Len when is_integer(Len) ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = #'BasicConstraints'{cA=true, pathLenConstraint=Len},
			 critical=true};
	_ ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = Data}
    end;
extension({Id, Data, Critical}) ->
    #'Extension'{extnID = Id, extnValue = Data, critical = Critical}.


publickey(#'RSAPrivateKey'{modulus=N, publicExponent=E}) ->
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    Algo = #'PublicKeyAlgorithm'{algorithm= ?rsaEncryption, parameters='NULL'},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = Public};
publickey(#'DSAPrivateKey'{p=P, q=Q, g=G, y=Y}) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ?'id-dsa', 
				 parameters={params, #'Dss-Parms'{p=P, q=Q, g=G}}},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo, subjectPublicKey = Y}.

validity(Opts) ->
    DefFrom0 = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())-1),
    DefTo0   = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())+7),
    {DefFrom, DefTo} = proplists:get_value(validity, Opts, {DefFrom0, DefTo0}),
    Format = fun({Y,M,D}) -> lists:flatten(io_lib:format("~w~2..0w~2..0w000000Z",[Y,M,D])) end,
    #'Validity'{notBefore={generalTime, Format(DefFrom)},
		notAfter ={generalTime, Format(DefTo)}}.

sign_algorithm(#'RSAPrivateKey'{}, Opts) ->
    Type = case proplists:get_value(digest, Opts, sha1) of
	       sha1 ->   ?'sha1WithRSAEncryption';
	       sha512 -> ?'sha512WithRSAEncryption';
	       sha384 -> ?'sha384WithRSAEncryption';
	       sha256 -> ?'sha256WithRSAEncryption';
	       md5    -> ?'md5WithRSAEncryption';
	       md2    -> ?'md2WithRSAEncryption'
	   end,
    {Type, 'NULL'};
sign_algorithm(#'DSAPrivateKey'{p=P, q=Q, g=G}, _Opts) ->
    {?'id-dsa-with-sha1', {params,#'Dss-Parms'{p=P, q=Q, g=G}}}.

make_key(rsa, _Opts) ->
    %% (OBS: for testing only)
    gen_rsa2(64);
make_key(dsa, _Opts) ->
    gen_dsa2(128, 20).  %% Bytes i.e. {1024, 160} 
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RSA key generation  (OBS: for testing only)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SMALL_PRIMES, [65537,97,89,83,79,73,71,67,61,59,53,
		       47,43,41,37,31,29,23,19,17,13,11,7,5,3]).

gen_rsa2(Size) ->
    P = prime(Size),
    Q = prime(Size),
    N = P*Q,
    Tot = (P - 1) * (Q - 1),
    [E|_] = lists:dropwhile(fun(Candidate) -> (Tot rem Candidate) == 0 end, ?SMALL_PRIMES),
    {D1,D2} = extended_gcd(E, Tot),
    D = erlang:max(D1,D2),
    case D < E of
	true ->
	    gen_rsa2(Size);
	false ->
	    {Co1,Co2} = extended_gcd(Q, P),
	    Co = erlang:max(Co1,Co2),
	    #'RSAPrivateKey'{version = 'two-prime',
			     modulus = N,
			     publicExponent  = E,
			     privateExponent = D, 
			     prime1 = P, 
			     prime2 = Q, 
			     exponent1 = D rem (P-1), 
			     exponent2 = D rem (Q-1), 
			     coefficient = Co
			    }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DSA key generation  (OBS: for testing only)
%% See http://en.wikipedia.org/wiki/Digital_Signature_Algorithm
%% and the fips_186-3.pdf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_dsa2(LSize, NSize) ->
    Q  = prime(NSize),  %% Choose N-bit prime Q
    X0 = prime(LSize),
    P0 = prime((LSize div 2) +1),
    
    %% Choose L-bit prime modulus P such that p-1 is a multiple of q.
    case dsa_search(X0 div (2*Q*P0), P0, Q, 1000) of
	error -> 
	    gen_dsa2(LSize, NSize);
	P ->	    
	    G = crypto:mod_exp(2, (P-1) div Q, P), % Choose G a number whose multiplicative order modulo p is q.
	    %%                 such that This may be done by setting g = h^(p-1)/q mod p, commonly h=2 is used.
	    
	    X = prime(20),               %% Choose x by some random method, where 0 < x < q.
	    Y = crypto:mod_exp(G, X, P), %% Calculate y = g^x mod p.
	    
	    #'DSAPrivateKey'{version=0, p=P, q=Q, g=G, y=Y, x=X}
    end.
    
%% See fips_186-3.pdf
dsa_search(T, P0, Q, Iter) when Iter > 0 ->
    P = 2*T*Q*P0 + 1,
    case is_prime(crypto:mpint(P), 50) of
	true -> P;
	false -> dsa_search(T+1, P0, Q, Iter-1)
    end;
dsa_search(_,_,_,_) -> 
    error.


%%%%%%% Crypto Math %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prime(ByteSize) ->
    Rand = odd_rand(ByteSize),
    crypto:erlint(prime_odd(Rand, 0)).

prime_odd(Rand, N) ->
    case is_prime(Rand, 50) of
	true -> 
	    Rand;
	false -> 
	    NotPrime = crypto:erlint(Rand),
	    prime_odd(crypto:mpint(NotPrime+2), N+1)
    end.

%% see http://en.wikipedia.org/wiki/Fermat_primality_test
is_prime(_, 0) -> true;
is_prime(Candidate, Test) -> 
    CoPrime = odd_rand(<<0,0,0,4, 10000:32>>, Candidate),
    case crypto:mod_exp(CoPrime, Candidate, Candidate) of
	CoPrime -> is_prime(Candidate, Test-1);
	_       -> false
    end.

odd_rand(Size) ->
    Min = 1 bsl (Size*8-1),
    Max = (1 bsl (Size*8))-1,
    odd_rand(crypto:mpint(Min), crypto:mpint(Max)).

odd_rand(Min,Max) ->
    Rand = <<Sz:32, _/binary>> = crypto:rand_uniform(Min,Max),
    BitSkip = (Sz+4)*8-1,
    case Rand of
	Odd  = <<_:BitSkip,  1:1>> -> Odd;
	Even = <<_:BitSkip,  0:1>> -> 
	    crypto:mpint(crypto:erlint(Even)+1)
    end.

extended_gcd(A, B) ->
    case A rem B of
	0 ->
	    {0, 1};
	N ->
	    {X, Y} = extended_gcd(B, N),
	    {Y, X-Y*(A div B)}
    end.

pem_to_der(File) ->
    {ok, PemBin} = file:read_file(File),
    public_key:pem_decode(PemBin).

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).
