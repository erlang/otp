-module(netconfc_test_lib).

-export([get_id_keys/1, remove_id_keys/1, make_dsa_files/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%%-----------------------------------------------------------------
%%% BEGIN SSH key management
%% copy private keys to given dir from ~/.ssh
get_id_keys(Config) ->
    DstDir = ?config(priv_dir, Config),
    SrcDir = filename:join(os:getenv("HOME"), ".ssh"),
    RsaOk = copyfile(SrcDir, DstDir, "id_rsa"),
    DsaOk = copyfile(SrcDir, DstDir, "id_dsa"),
    case {RsaOk, DsaOk} of
	{{ok, _}, {ok, _}} -> {ok, both};
	{{ok, _}, _} -> {ok, rsa};
	{_, {ok, _}} -> {ok, dsa};
	{Error, _} -> Error
    end.

%% Remove later on. Use make_dsa_files instead.
remove_id_keys(Config) ->
    Dir = ?config(priv_dir, Config),
    file:delete(filename:join(Dir, "id_rsa")),
    file:delete(filename:join(Dir, "id_dsa")).


make_dsa_files(Config) ->
    make_dsa_files(Config, rfc4716_public_key).
make_dsa_files(Config, Type) ->
    {DSA, EncodedKey} = gen_dsa(128, 20),
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
%% @doc Creates a dsa key (OBS: for testing only)
%%   the sizes are in bytes
%% @spec (::integer()) -> {::atom(), ::binary(), ::opaque()}
%% @end
%%--------------------------------------------------------------------
gen_dsa(LSize,NSize) when is_integer(LSize), is_integer(NSize) ->
    Key = gen_dsa2(LSize, NSize),
    {Key, encode_key(Key)}.

encode_key(Key = #'DSAPrivateKey'{}) ->
    Der = public_key:der_encode('DSAPrivateKey', Key),
    {'DSAPrivateKey', Der, not_encrypted}.

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
	    G = crypto:mod_pow(2, (P-1) div Q, P), % Choose G a number whose multiplicative order modulo p is q.
	    %%                 such that This may be done by setting g = h^(p-1)/q mod p, commonly h=2 is used.
	    
	    X = prime(20),               %% Choose x by some random method, where 0 < x < q.
	    Y = crypto:mod_pow(G, X, P), %% Calculate y = g^x mod p.
	    
	    #'DSAPrivateKey'{version=0, p = P, q = Q, 
			     g = crypto:bytes_to_integer(G), y = crypto:bytes_to_integer(Y), x = X}
    end.
    
%% See fips_186-3.pdf
dsa_search(T, P0, Q, Iter) when Iter > 0 ->
    P = 2*T*Q*P0 + 1,
    case is_prime(P, 50) of
	true -> P;
	false -> dsa_search(T+1, P0, Q, Iter-1)
    end;
dsa_search(_,_,_,_) -> 
    error.


%%%%%%% Crypto Math %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prime(ByteSize) ->
    Rand = odd_rand(ByteSize),
    prime_odd(Rand, 0).

prime_odd(Rand, N) ->
    case is_prime(Rand, 50) of
	true -> 
	    Rand;
	false -> 
	    prime_odd(Rand+2, N+1)
    end.

%% see http://en.wikipedia.org/wiki/Fermat_primality_test
is_prime(_, 0) -> true;
is_prime(Candidate, Test) -> 
    CoPrime = odd_rand(10000, Candidate),
    Result = crypto:mod_pow(CoPrime, Candidate, Candidate) ,
    is_prime(CoPrime, crypto:bytes_to_integer(Result), Candidate, Test).

is_prime(CoPrime, CoPrime, Candidate, Test) ->
    is_prime(Candidate, Test-1);
is_prime(_,_,_,_) ->
    false.

odd_rand(Size) ->
    Min = 1 bsl (Size*8-1),
    Max = (1 bsl (Size*8))-1,
    odd_rand(Min, Max).

odd_rand(Min,Max) ->
    Rand = crypto:rand_uniform(Min,Max),
    case Rand rem 2 of
	0 -> 
	    Rand + 1;
	_ -> 
	    Rand
    end.

copyfile(SrcDir, DstDir, Fn) ->
    file:copy(filename:join(SrcDir, Fn),
	      filename:join(DstDir, Fn)).

%%% END SSH key management
%%%-----------------------------------------------------------------
