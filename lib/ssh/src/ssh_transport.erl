%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2023. All Rights Reserved.
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

%%% Description: SSH transport protocol

-module(ssh_transport).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/inet.hrl").

-include("ssh_transport.hrl").
-include("ssh.hrl").

-export([versions/2, hello_version_msg/1]).
-export([next_seqnum/1, 
	 supported_algorithms/0, supported_algorithms/1,
	 default_algorithms/0, default_algorithms/1,
         clear_default_algorithms_env/0,
         algo_classes/0, algo_class/1,
         algo_two_spec_classes/0, algo_two_spec_class/1,
	 handle_packet_part/5,
	 handle_hello_version/1,
	 key_exchange_init_msg/1,
	 key_init/3, new_keys_message/1,
         ext_info_message/1,
	 handle_kexinit_msg/3, handle_kexdh_init/2,
	 handle_kex_dh_gex_group/2, handle_kex_dh_gex_init/2, handle_kex_dh_gex_reply/2,
	 handle_new_keys/2, handle_kex_dh_gex_request/2,
	 handle_kexdh_reply/2, 
	 handle_kex_ecdh_init/2,
	 handle_kex_ecdh_reply/2,
         parallell_gen_key/1,
	 ssh_packet/2, pack/2,
         valid_key_sha_alg/3,
	 sign/3, sign/4,
         verify/5,
	 sha/1,
         get_host_key/2,
         call_KeyCb/3,
         public_algo/1]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2]).

-define(MIN_DH_KEY_SIZE, 400).

%%% For test suites
-export([pack/3, adjust_algs_for_peer_version/2]).

%%%----------------------------------------------------------------------------
%%%
%%% There is a difference between supported and default algorithms. The
%%% SUPPORTED algorithms can be handled (maybe untested...). The DEFAULT ones
%%% are announced in ssh_msg_kexinit and in ssh:default_algorithms/0 to the
%%% user.
%%%
%%% A supported algorithm can be requested in the option 'preferred_algorithms',
%%% but may give unexpected results before being promoted to default.
%%%
%%% This makes it possible to add experimental algorithms (in supported_algorithms)
%%% and test them without letting the default users know about them.
%%%

-define(DEFAULT_ALGS, '$def-algs$').

clear_default_algorithms_env() ->
    application:unset_env(ssh, ?DEFAULT_ALGS).

-spec default_algorithms() -> algs_list()
                                  | no_return() %  error(Reason)
                                  .
default_algorithms() ->
    FipsMode = crypto:info_fips(),
    case application:get_env(ssh, ?DEFAULT_ALGS) of
        undefined ->
            Algs = build_cache(),
            application:set_env(ssh, ?DEFAULT_ALGS, {FipsMode,Algs}),
            Algs;

        {ok,{FipsMode,Algs}} ->
            %% Cached, and the FIPS mode is the same now as when it was cached.
            Algs;

        {ok,{_OtherFipsMode,_Algs}} ->
            %% Cached, but the FIPS mode has changed.
            Algs = build_cache(),
            application:set_env(ssh, ?DEFAULT_ALGS, {FipsMode,Algs}),
            Algs
    end.

build_cache() ->
    Opts = get_alg_conf(),
    Algs1 =
        case proplists:get_value(preferred_algorithms, Opts) of
            undefined ->
                [{K,default_algorithms1(K)} || K <- algo_classes()];
            Algs0 ->
                {true,Algs01} = ssh_options:check_preferred_algorithms(Algs0),
                Algs01
        end,
    Algs =
        case proplists:get_value(modify_algorithms, Opts) of
            undefined ->
                Algs1;
            Modifications ->
                ssh_options:initial_default_algorithms(Algs1, Modifications)
        end,
    Algs.



get_alg_conf() ->
    [{T,L} || T <- [preferred_algorithms, modify_algorithms],
              L <- [application:get_env(ssh, T, [])],
              L =/= []].

algo_classes() -> [kex, public_key, cipher, mac, compression].

algo_class(kex) -> true;
algo_class(public_key) -> true;
algo_class(cipher) -> true;
algo_class(mac) -> true;
algo_class(compression) -> true;
algo_class(_) -> false.


algo_two_spec_classes() -> [cipher, mac, compression].

algo_two_spec_class(cipher) -> true;
algo_two_spec_class(mac) -> true;
algo_two_spec_class(compression) -> true;
algo_two_spec_class(_) -> false.


default_algorithms(Tag) ->
    FipsMode = crypto:info_fips(),
    case application:get_env(ssh, ?DEFAULT_ALGS) of
        undefined ->
            default_algorithms1(Tag);
        {ok,{FipsMode,Algs}} ->
            %% Cached, and the FIPS mode is the same now as when it was cached.
            proplists:get_value(Tag, Algs, []);
        {ok,{_OtherFipsMode,_Algs}} ->
            %% Cached, but the FIPS mode has changed.
            Algs = build_cache(),
            application:set_env(ssh, ?DEFAULT_ALGS, {FipsMode,Algs}),
            proplists:get_value(Tag, Algs, [])
    end.
    

default_algorithms1(kex) ->
    supported_algorithms(kex, [
                               %%  Gone in OpenSSH 7.3.p1:
                               'diffie-hellman-group1-sha1',
                               %%  Gone in OpenSSH 8.2
                               'diffie-hellman-group14-sha1',
                               'diffie-hellman-group-exchange-sha1'
                              ]);

default_algorithms1(cipher) ->
    supported_algorithms(cipher, same(['AEAD_AES_128_GCM',
				       'AEAD_AES_256_GCM'
                                      ]));
default_algorithms1(mac) ->
    supported_algorithms(mac, same(['AEAD_AES_128_GCM',
				    'AEAD_AES_256_GCM',
                                    'hmac-sha1-96'
                                   ]));

default_algorithms1(public_key) ->
    supported_algorithms(public_key, [
                                      'ssh-rsa',
                                      %% Gone in OpenSSH 7.3.p1:
                                      'ssh-dss'
                                     ]);

default_algorithms1(Alg) ->
    supported_algorithms(Alg, []).


supported_algorithms() -> [{K,supported_algorithms(K)} || K <- algo_classes()].

supported_algorithms(kex) ->
    select_crypto_supported(
      [
       {'ecdh-sha2-nistp384',                   [{public_keys,ecdh}, {curves,secp384r1}, {hashs,sha384}]},
       {'ecdh-sha2-nistp521',                   [{public_keys,ecdh}, {curves,secp521r1}, {hashs,sha512}]},
       {'ecdh-sha2-nistp256',                   [{public_keys,ecdh}, {curves,secp256r1}, {hashs,sha256}]},
       {'diffie-hellman-group-exchange-sha256', [{public_keys,dh},   {hashs,sha256}]},
       {'diffie-hellman-group16-sha512',        [{public_keys,dh},   {hashs,sha512}]}, % In OpenSSH 7.3.p1
       {'diffie-hellman-group18-sha512',        [{public_keys,dh},   {hashs,sha512}]}, % In OpenSSH 7.3.p1
       {'diffie-hellman-group14-sha256',        [{public_keys,dh},   {hashs,sha256}]}, % In OpenSSH 7.3.p1
       %% https://tools.ietf.org/html/draft-ietf-curdle-ssh-curves
       %% Secure Shell (SSH) Key Exchange Method using Curve25519 and Curve448
       {'curve25519-sha256',                    [{public_keys,ecdh}, {curves,x25519}, {hashs,sha256}]},
       {'curve25519-sha256@libssh.org',         [{public_keys,ecdh}, {curves,x25519}, {hashs,sha256}]},
       {'curve448-sha512',                      [{public_keys,ecdh}, {curves,x448},   {hashs,sha512}]},
       {'diffie-hellman-group14-sha1',          [{public_keys,dh},   {hashs,sha}]},
       {'diffie-hellman-group-exchange-sha1',   [{public_keys,dh},   {hashs,sha}]},
       {'diffie-hellman-group1-sha1',           [{public_keys,dh},   {hashs,sha}]}
      ]);
supported_algorithms(public_key) ->
    select_crypto_supported(
      [
       {'ecdsa-sha2-nistp384',  [{public_keys,ecdsa}, {hashs,sha384}, {curves,secp384r1}]},
       {'ecdsa-sha2-nistp521',  [{public_keys,ecdsa}, {hashs,sha512}, {curves,secp521r1}]},
       {'ecdsa-sha2-nistp256',  [{public_keys,ecdsa}, {hashs,sha256}, {curves,secp256r1}]},
       {'ssh-ed25519',          [{public_keys,eddsa}, {curves,ed25519}                    ]},
       {'ssh-ed448',            [{public_keys,eddsa}, {curves,ed448}                      ]},
       {'rsa-sha2-256',         [{public_keys,rsa},   {hashs,sha256}                      ]},
       {'rsa-sha2-512',         [{public_keys,rsa},   {hashs,sha512}                      ]},
       {'ssh-rsa',              [{public_keys,rsa},   {hashs,sha}                         ]},
       {'ssh-dss',              [{public_keys,dss},   {hashs,sha}                         ]} % Gone in OpenSSH 7.3.p1
      ]);
 
supported_algorithms(cipher) ->
    same(
      select_crypto_supported(
	[
         {'chacha20-poly1305@openssh.com', [{ciphers,chacha20}, {macs,poly1305}]},
         {'aes256-gcm@openssh.com', [{ciphers,aes_256_gcm}]},
         {'aes256-ctr',       [{ciphers,aes_256_ctr}]},
         {'aes192-ctr',       [{ciphers,aes_192_ctr}]},
	 {'aes128-gcm@openssh.com', [{ciphers,aes_128_gcm}]},
	 {'aes128-ctr',       [{ciphers,aes_128_ctr}]},
	 {'AEAD_AES_256_GCM', [{ciphers,aes_256_gcm}]},
	 {'AEAD_AES_128_GCM', [{ciphers,aes_128_gcm}]},
	 {'aes256-cbc',       [{ciphers,aes_256_cbc}]},
	 {'aes192-cbc',       [{ciphers,aes_192_cbc}]},
	 {'aes128-cbc',       [{ciphers,aes_128_cbc}]},
	 {'3des-cbc',         [{ciphers,des_ede3_cbc}]}
	]
       ));
supported_algorithms(mac) ->
    same(
      select_crypto_supported(
	[{'hmac-sha2-256-etm@openssh.com', [{macs,hmac}, {hashs,sha256}]},
         {'hmac-sha2-512-etm@openssh.com', [{macs,hmac}, {hashs,sha256}]},
         {'hmac-sha2-256',    [{macs,hmac}, {hashs,sha256}]},
	 {'hmac-sha2-512',    [{macs,hmac}, {hashs,sha512}]},
         {'hmac-sha1-etm@openssh.com', [{macs,hmac}, {hashs,sha256}]},
	 {'hmac-sha1',        [{macs,hmac}, {hashs,sha}]},
	 {'hmac-sha1-96',     [{macs,hmac}, {hashs,sha}]},
	 {'AEAD_AES_128_GCM', [{ciphers,aes_128_gcm}]},
	 {'AEAD_AES_256_GCM', [{ciphers,aes_256_gcm}]}
	]
       ));
supported_algorithms(compression) ->
    same(['none',
 	  'zlib@openssh.com',
	  'zlib'
	 ]).

%%%----------------------------------------------------------------------------
versions(client, Options)->
    Vsn = ?GET_INTERNAL_OPT(vsn, Options, ?DEFAULT_CLIENT_VERSION),
    {Vsn, format_version(Vsn, software_version(Options))};
versions(server, Options) ->
    Vsn = ?GET_INTERNAL_OPT(vsn, Options, ?DEFAULT_SERVER_VERSION),
    {Vsn, format_version(Vsn, software_version(Options))}.

format_version({Major,Minor}, "") ->
    lists:concat(["SSH-",Major,".",Minor]);
format_version({Major,Minor}, SoftwareVersion) ->
    lists:concat(["SSH-",Major,".",Minor,"-",SoftwareVersion]).

software_version(Options) -> 
    case ?GET_OPT(id_string, Options) of
	{random,Nlo,Nup} ->
	    random_id(Nlo,Nup);
	ID ->
	    ID
    end.

random_id(Nlo, Nup) ->
    [$a + rand:uniform($z-$a+1) - 1 || _<- lists:duplicate(Nlo + rand:uniform(Nup-Nlo+1) - 1, x)].

hello_version_msg(Data) ->
    [Data,"\r\n"].

next_seqnum(SeqNum) ->
    (SeqNum + 1) band 16#ffffffff.

is_valid_mac(_, _ , #ssh{recv_mac_size = 0}) ->
    true;
is_valid_mac(Mac, Data, #ssh{recv_mac = Algorithm,
			     recv_mac_key = Key, recv_sequence = SeqNum}) ->
    ssh_lib:comp(Mac, mac(Algorithm, Key, SeqNum, Data)).

handle_hello_version(Version) ->
    try
	StrVersion = trim_tail(Version),
	case string:tokens(Version, "-") of
	    [_, "2.0" | _] ->
		{{2,0}, StrVersion};
	    [_, "1.99" | _] ->
		{{2,0}, StrVersion};
	    [_, "1.3" | _] ->
		{{1,3}, StrVersion};
	    [_, "1.5" | _] ->
		{{1,5}, StrVersion}
	end
    catch
	error:_ ->
	    {undefined, "unknown version"}
    end.

key_exchange_init_msg(Ssh0) ->
    Msg = kex_init(Ssh0),
    {SshPacket, Ssh} = ssh_packet(Msg, Ssh0),
    {Msg, SshPacket, Ssh}.

kex_init(#ssh{role = Role, opts = Opts, available_host_keys = HostKeyAlgs} = Ssh) ->
    Random = ssh_bits:random(16),
    PrefAlgs = adjust_algs_for_peer_version(Role, ?GET_OPT(preferred_algorithms, Opts), Ssh),
    kexinit_message(Role, Random, PrefAlgs, HostKeyAlgs, Opts).

key_init(client, Ssh, Value) ->
    Ssh#ssh{c_keyinit = Value};
key_init(server, Ssh, Value) ->
    Ssh#ssh{s_keyinit = Value}.

adjust_algs_for_peer_version(client, PrefAlgs, #ssh{s_version=V}) ->
    adjust_algs_for_peer_version(V, PrefAlgs);
adjust_algs_for_peer_version(server, PrefAlgs, #ssh{c_version=V}) ->
    adjust_algs_for_peer_version(V, PrefAlgs).
%%
adjust_algs_for_peer_version("SSH-2.0-OpenSSH_6.2"++_, PrefAlgs) ->
    C0 = proplists:get_value(cipher, PrefAlgs, same([])),
    C = [{D,L} || D <- [client2server, server2client],
                  L <- [[K || K <- proplists:get_value(D, C0, []),
                              K =/= 'aes256-gcm@openssh.com',
                              K =/= 'aes128-gcm@openssh.com']]
        ],
    lists:keyreplace(cipher, 1, PrefAlgs, {cipher,C});
adjust_algs_for_peer_version(_, PrefAlgs) ->
    PrefAlgs.
    
kexinit_message(Role, Random, Algs, HostKeyAlgs, Opts) ->
    #ssh_msg_kexinit{
		  cookie = Random,
		  kex_algorithms = to_strings( get_algs(kex,Algs) )
                                   ++ kex_ext_info(Role,Opts),
		  server_host_key_algorithms = HostKeyAlgs,
		  encryption_algorithms_client_to_server = c2s(cipher,Algs),
		  encryption_algorithms_server_to_client = s2c(cipher,Algs),
		  mac_algorithms_client_to_server = c2s(mac,Algs),
		  mac_algorithms_server_to_client = s2c(mac,Algs),
		  compression_algorithms_client_to_server = c2s(compression,Algs),
		  compression_algorithms_server_to_client = s2c(compression,Algs),
		  languages_client_to_server = [],
		  languages_server_to_client = []
		 }.

c2s(Key, Algs) -> x2y(client2server, Key, Algs).
s2c(Key, Algs) -> x2y(server2client, Key, Algs).

x2y(DirectionKey, Key, Algs) -> to_strings(proplists:get_value(DirectionKey, get_algs(Key,Algs))).

get_algs(Key, {_FipsMode,Algs}) when is_list(Algs) ->  proplists:get_value(Key, Algs, default_algorithms(Key));
get_algs(Key, Algs) when is_list(Algs) -> proplists:get_value(Key, Algs, default_algorithms(Key)).

to_strings(L) -> lists:map(fun erlang:atom_to_list/1, L).

new_keys_message(Ssh0) ->
    {SshPacket, Ssh1} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
    Ssh = install_alg(snd, Ssh1),
    {ok, SshPacket, Ssh}.


handle_kexinit_msg(#ssh_msg_kexinit{} = CounterPart, #ssh_msg_kexinit{} = Own,
                   #ssh{role = client} = Ssh) ->
    try
        {ok, Algorithms} = select_algorithm(client, Own, CounterPart, Ssh#ssh.opts),
        true = verify_algorithm(Algorithms),
        Algorithms
    of
	Algos ->
	    key_exchange_first_msg(Algos#alg.kex, 
				   Ssh#ssh{algorithms = Algos})
    catch
        Class:Error ->
            Msg = kexinit_error(Class, Error, client, Own, CounterPart),
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED, Msg)
        end;

handle_kexinit_msg(#ssh_msg_kexinit{} = CounterPart, #ssh_msg_kexinit{} = Own,
                   #ssh{role = server} = Ssh) ->
    try
        {ok, Algorithms} = select_algorithm(server, CounterPart, Own, Ssh#ssh.opts),
        true = verify_algorithm(Algorithms),
        Algorithms
    of
	Algos ->
            {ok, Ssh#ssh{algorithms = Algos}}
    catch
        Class:Error ->
            Msg = kexinit_error(Class, Error, server, Own, CounterPart),
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED, Msg)
    end.

kexinit_error(Class, Error, Role, Own, CounterPart) ->
    {Fmt,Args} =
        case {Class,Error} of
            {error, {badmatch,{false,Alg}}} ->
                {Txt,W,C} = alg_info(Role, Alg),
                {"No common ~s algorithm,~n"
                 "  we have:~n    ~s~n"
                 "  peer have:~n    ~s~n",
                 [Txt,
                  lists:join(", ", element(W,Own)),
                  lists:join(", ", element(C,CounterPart))
                 ]};
            _ ->
                {"Kexinit failed in ~p: ~p:~p", [Role,Class,Error]}
        end,
    try io_lib:format(Fmt, Args) of
        R -> R
    catch
        _:_ ->
            io_lib:format("Kexinit failed in ~p: ~p:~p", [Role, Class, Error])
    end.

alg_info(client, Alg) ->
    alg_info(Alg);
alg_info(server, Alg) ->
    {Txt,C2s,S2c} = alg_info(Alg),
    {Txt,S2c,C2s}.

alg_info("kex") ->        {"key exchange",
                           #ssh_msg_kexinit.kex_algorithms,
                           #ssh_msg_kexinit.kex_algorithms};
alg_info("hkey") ->       {"key",
                           #ssh_msg_kexinit.server_host_key_algorithms,
                           #ssh_msg_kexinit.server_host_key_algorithms};
alg_info("send_mac") ->    {"mac",
                           #ssh_msg_kexinit.mac_algorithms_client_to_server,
                           #ssh_msg_kexinit.mac_algorithms_server_to_client};
alg_info("recv_mac") ->    {"mac",
                           #ssh_msg_kexinit.mac_algorithms_client_to_server,
                           #ssh_msg_kexinit.mac_algorithms_server_to_client};
alg_info("encrypt") ->   {"cipher",
                           #ssh_msg_kexinit.encryption_algorithms_client_to_server,
                           #ssh_msg_kexinit.encryption_algorithms_server_to_client};
alg_info("decrypt") ->   {"cipher",
                           #ssh_msg_kexinit.encryption_algorithms_client_to_server,
                           #ssh_msg_kexinit.encryption_algorithms_server_to_client};
alg_info("compress") ->   {"compress",
                           #ssh_msg_kexinit.compression_algorithms_client_to_server,
                           #ssh_msg_kexinit.compression_algorithms_server_to_client};
alg_info("decompress") -> {"compress",
                           #ssh_msg_kexinit.compression_algorithms_client_to_server,
                           #ssh_msg_kexinit.compression_algorithms_server_to_client}.


verify_algorithm(#alg{kex = undefined})       ->  {false, "kex"};
verify_algorithm(#alg{hkey = undefined})      ->  {false, "hkey"};
verify_algorithm(#alg{send_mac = undefined})  ->  {false, "send_mac"};
verify_algorithm(#alg{recv_mac = undefined})  ->  {false, "recv_mac"};
verify_algorithm(#alg{encrypt = undefined})   ->  {false, "encrypt"};
verify_algorithm(#alg{decrypt = undefined})   ->  {false, "decrypt"};
verify_algorithm(#alg{compress = undefined})  ->  {false, "compress"};
verify_algorithm(#alg{decompress = undefined}) -> {false, "decompress"};
verify_algorithm(#alg{kex = Kex}) -> 
    %% This also catches the error if 'ext-info-s' or 'ext-info-c' is selected.
    %% (draft-ietf-curdle-ssh-ext-info-04 2.2)
    case lists:member(Kex, supported_algorithms(kex)) of
        true -> true;
        false -> {false, "kex"}
    end.

%%%----------------------------------------------------------------
%%%
%%% Key exchange initialization
%%%
key_exchange_first_msg(Kex, Ssh0) when Kex == 'diffie-hellman-group1-sha1' ;
				       Kex == 'diffie-hellman-group14-sha1' ;
                                       Kex == 'diffie-hellman-group14-sha256' ;
                                       Kex == 'diffie-hellman-group16-sha512' ;
                                       Kex == 'diffie-hellman-group18-sha512'
                                       ->
    {G, P} = dh_group(Kex),
    Sz = dh_bits(Ssh0#ssh.algorithms),
    {Public, Private} = generate_key(dh, [P,G,2*Sz]),
    {SshPacket, Ssh1} = ssh_packet(#ssh_msg_kexdh_init{e = Public}, Ssh0),
    {ok, SshPacket, 
     Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}}}};

key_exchange_first_msg(Kex, Ssh0=#ssh{opts=Opts}) when Kex == 'diffie-hellman-group-exchange-sha1' ;
						       Kex == 'diffie-hellman-group-exchange-sha256' ->
    {Min,NBits0,Max} = ?GET_OPT(dh_gex_limits, Opts),
    DhBits = dh_bits(Ssh0#ssh.algorithms),
    NBits1 = 
        %% NIST Special Publication 800-57 Part 1 Revision 4: Recommendation for Key Management
        if 
            DhBits =< 112 -> 2048;
            DhBits =< 128 -> 3072;
            DhBits =< 192 -> 7680;
            true -> 8192
        end,
    NBits = min(max(max(NBits0,NBits1),Min), Max),
    
    {SshPacket, Ssh1} = 
	ssh_packet(#ssh_msg_kex_dh_gex_request{min = Min, 
					       n = NBits,
					       max = Max}, 
		   Ssh0),
    {ok, SshPacket, 
     Ssh1#ssh{keyex_info = {Min, Max, NBits}}};

key_exchange_first_msg(Kex, Ssh0) when Kex == 'ecdh-sha2-nistp256' ;
				       Kex == 'ecdh-sha2-nistp384' ;
				       Kex == 'ecdh-sha2-nistp521' ;
                                       Kex == 'curve25519-sha256' ;
                                       Kex == 'curve25519-sha256@libssh.org';
                                       Kex == 'curve448-sha512' ->
    Curve = ecdh_curve(Kex),
    {Public, Private} = generate_key(ecdh, Curve),
    {SshPacket, Ssh1} = ssh_packet(#ssh_msg_kex_ecdh_init{q_c=Public},  Ssh0),
    {ok, SshPacket, 
     Ssh1#ssh{keyex_key = {{Public,Private},Curve}}}.

%%%----------------------------------------------------------------
%%%
%%% diffie-hellman-group1-sha1
%%% diffie-hellman-group14-sha1
%%% diffie-hellman-group14-sha256
%%% diffie-hellman-group16-sha512
%%% diffie-hellman-group18-sha512
%%% 
handle_kexdh_init(#ssh_msg_kexdh_init{e = E}, 
		  Ssh0 = #ssh{algorithms = #alg{kex=Kex,
                                                hkey=SignAlg} = Algs,
                              opts = Opts}) ->
    %% server
    {G, P} = dh_group(Kex),
    if
	1=<E, E=<(P-1) ->
            Sz = dh_bits(Algs),
	    {Public, Private} = generate_key(dh, [P,G,2*Sz]),
	    K = compute_key(dh, E, Private, [P,G]),
	    MyPrivHostKey = get_host_key(SignAlg, Opts),
	    MyPubHostKey = ssh_file:extract_public_key(MyPrivHostKey),
            H = kex_hash(Ssh0, MyPubHostKey, sha(Kex), {E,Public,K}),
            case sign(H, SignAlg, MyPrivHostKey, Ssh0) of
                {ok,H_SIG} ->
                    {SshPacket, Ssh1} =
                        ssh_packet(#ssh_msg_kexdh_reply{public_host_key = {MyPubHostKey,SignAlg},
                                                        f = Public,
                                                        h_sig = H_SIG
                                                       }, Ssh0),
                    {ok, SshPacket, Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}},
                                             shared_secret = ssh_bits:mpint(K),
                                             exchanged_hash = H,
                                             session_id = sid(Ssh1, H)}};
                {error,unsupported_sign_alg} ->
                    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                io_lib:format("Unsupported algorithm ~p", [SignAlg])
                               )
            end;
	true ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("Kexdh init failed, received 'e' out of bounds~n  E=~p~n  P=~p",
                                      [E,P])
                       )
    end.

handle_kexdh_reply(#ssh_msg_kexdh_reply{public_host_key = PeerPubHostKey,
					f = F,
					h_sig = H_SIG}, 
		   #ssh{keyex_key = {{Private, Public}, {G, P}},
                        algorithms = #alg{kex=Kex}} = Ssh0) ->
    %% client
    if 
	1=<F, F=<(P-1)->
	    K = compute_key(dh, F, Private, [P,G]),
            H = kex_hash(Ssh0, PeerPubHostKey, sha(Kex), {Public,F,K}),
	    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
		ok ->
		    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
		    {ok, SshPacket, install_alg(snd, Ssh#ssh{shared_secret  = ssh_bits:mpint(K),
                                                             exchanged_hash = H,
                                                             session_id = sid(Ssh, H)})};
		Error ->
                    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                io_lib:format("Kexdh init failed. Verify host key: ~p",[Error])
                               )
	    end;

	true ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("Kexdh init failed, received 'f' out of bounds~n  F=~p~n  P=~p",
                                      [F,P])
                       )
    end.


%%%----------------------------------------------------------------
%%%
%%% diffie-hellman-group-exchange-sha1
%%% 
handle_kex_dh_gex_request(#ssh_msg_kex_dh_gex_request{min = Min0,
						      n   = NBits,
						      max = Max0}, 
			  Ssh0=#ssh{opts=Opts}) when Min0=<NBits, NBits=<Max0 ->
    %% server
    {Min, Max} = adjust_gex_min_max(Min0, Max0, Opts),
    case public_key:dh_gex_group(Min, NBits, Max,
				 ?GET_OPT(dh_gex_groups,Opts)) of
	{ok, {_, {G,P}}} ->
	    {SshPacket, Ssh} = 
		ssh_packet(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0),
	    {ok, SshPacket, 
             Ssh#ssh{keyex_key = {x, {G, P}},
		     keyex_info = {Min0, Max0, NBits}
		    }};
	{error,_} ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("No possible diffie-hellman-group-exchange group found",[])
                       )
    end;

handle_kex_dh_gex_request(#ssh_msg_kex_dh_gex_request_old{n = NBits}, 
			  Ssh0=#ssh{opts=Opts}) ->
    %% server
    %%
    %% This message was in the draft-00 of rfc4419
    %% (https://tools.ietf.org/html/draft-ietf-secsh-dh-group-exchange-00)
    %% In later drafts and the rfc is "is used for backward compatibility".
    %% Unfortunately the rfc does not specify how to treat the parameter n
    %% if there is no group of that modulus length :(
    %% The draft-00 however specifies that n is the "... number of bits
    %% the subgroup should have at least".
    %% Further, it says that "Servers and clients SHOULD support groups
    %% with a modulus length of k bits, where 1024 <= k <= 8192."
    %%
    Min0 = NBits,
    Max0 = 8192,
    {Min, Max} = adjust_gex_min_max(Min0, Max0, Opts),
    case public_key:dh_gex_group(Min, NBits, Max,
				 ?GET_OPT(dh_gex_groups,Opts)) of
	{ok, {_, {G,P}}} ->
	    {SshPacket, Ssh} = 
		ssh_packet(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0),
	    {ok, SshPacket, 
	     Ssh#ssh{keyex_key = {x, {G, P}},
		     keyex_info = {-1, -1, NBits} % flag for kex_hash calc
		    }};
	{error,_} ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("No possible diffie-hellman-group-exchange group found",[])
                       )
    end;

handle_kex_dh_gex_request(_, _) ->
    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                "Key exchange failed, bad values in ssh_msg_kex_dh_gex_request").

adjust_gex_min_max(Min0, Max0, Opts) ->
    {Min1, Max1} = ?GET_OPT(dh_gex_limits, Opts),
    Min2 = max(Min0, Min1),
    Max2 = min(Max0, Max1),
    if
        Min2 =< Max2 ->
            {Min2, Max2};
        Max2 < Min2 ->
            ?DISCONNECT(?SSH_DISCONNECT_PROTOCOL_ERROR,
                        "No possible diffie-hellman-group-exchange group possible")
    end.
		    

handle_kex_dh_gex_group(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0) ->
    %% client
    Sz = dh_bits(Ssh0#ssh.algorithms),
    {Public, Private} = generate_key(dh, [P,G,2*Sz]),
    {SshPacket, Ssh1} = 
	ssh_packet(#ssh_msg_kex_dh_gex_init{e = Public}, Ssh0),	% Pub = G^Priv mod P (def)

    {ok, SshPacket, 
     Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}}}}.

handle_kex_dh_gex_init(#ssh_msg_kex_dh_gex_init{e = E}, 
		       #ssh{keyex_key = {{Private, Public}, {G, P}},
			    keyex_info = {Min, Max, NBits},
                            algorithms = #alg{kex=Kex,
                                              hkey=SignAlg},
                            opts = Opts} = Ssh0) ->
    %% server
    if
	1=<E, E=<(P-1) ->
	    K = compute_key(dh, E, Private, [P,G]),
	    if
		1<K, K<(P-1) ->
		    MyPrivHostKey = get_host_key(SignAlg, Opts),
		    MyPubHostKey = ssh_file:extract_public_key(MyPrivHostKey),
                    H = kex_hash(Ssh0, MyPubHostKey, sha(Kex), {Min,NBits,Max,P,G,E,Public,K}),
                    case sign(H, SignAlg, MyPrivHostKey, Ssh0) of
                        {ok,H_SIG} ->
                            {SshPacket, Ssh} =
                                ssh_packet(#ssh_msg_kex_dh_gex_reply{public_host_key = {MyPubHostKey,SignAlg},
                                                                     f = Public,
                                                                     h_sig = H_SIG}, Ssh0),
                            {ok, SshPacket, Ssh#ssh{shared_secret = ssh_bits:mpint(K),
                                                    exchanged_hash = H,
                                                    session_id = sid(Ssh, H)
                                                   }};
                        {error,unsupported_sign_alg} ->
                            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                        io_lib:format("Unsupported algorithm ~p", [SignAlg])
                                       )
                    end;
		true ->
                    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                "Kexdh init failed, received 'k' out of bounds"
                               )
	    end;
	true ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("Kexdh gex init failed, received 'e' out of bounds~n  E=~p~n  P=~p",
                                      [E,P])
                       )
    end.

handle_kex_dh_gex_reply(#ssh_msg_kex_dh_gex_reply{public_host_key = PeerPubHostKey, 
						  f = F,
						  h_sig = H_SIG},
			#ssh{keyex_key = {{Private, Public}, {G, P}},
			     keyex_info = {Min, Max, NBits},
                             algorithms = #alg{kex=Kex}} = 
			    Ssh0) ->
    %% client
    if 
	1=<F, F=<(P-1)->
	    K = compute_key(dh, F, Private, [P,G]),
	    if
		1<K, K<(P-1) ->
                    H = kex_hash(Ssh0, PeerPubHostKey, sha(Kex), {Min,NBits,Max,P,G,Public,F,K}),
		    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
			ok ->
			    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
			    {ok, SshPacket, install_alg(snd, Ssh#ssh{shared_secret  = ssh_bits:mpint(K),
                                                                     exchanged_hash = H,
                                                                     session_id = sid(Ssh, H)})};
                        Error ->
                            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                        io_lib:format("Kexdh gex reply failed. Verify host key: ~p",[Error])
                                       )
		    end;

		true ->
                    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                "Kexdh gex init failed, 'K' out of bounds"
                               )
	    end;
	true ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("Kexdh gex init failed, received 'f' out of bounds~n  F=~p~n  P=~p",
                                      [F,P])
                       )
    end.

%%%----------------------------------------------------------------
%%%
%%% diffie-hellman-ecdh-sha2-*
%%% 
handle_kex_ecdh_init(#ssh_msg_kex_ecdh_init{q_c = PeerPublic},
		     Ssh0 = #ssh{algorithms = #alg{kex=Kex,
                                                   hkey=SignAlg},
                                 opts = Opts}) ->
    %% at server
    Curve = ecdh_curve(Kex),
    {MyPublic, MyPrivate} = generate_key(ecdh, Curve),
    try
	compute_key(ecdh, PeerPublic, MyPrivate, Curve)
    of
	K ->
	    MyPrivHostKey = get_host_key(SignAlg, Opts),
	    MyPubHostKey = ssh_file:extract_public_key(MyPrivHostKey),
            H = kex_hash(Ssh0, MyPubHostKey, sha(Curve), {PeerPublic, MyPublic, K}),
            case sign(H, SignAlg, MyPrivHostKey, Ssh0) of
                {ok,H_SIG} ->
                    {SshPacket, Ssh1} =
                        ssh_packet(#ssh_msg_kex_ecdh_reply{public_host_key = {MyPubHostKey,SignAlg},
                                                           q_s = MyPublic,
                                                           h_sig = H_SIG},
                                   Ssh0),
                    {ok, SshPacket, Ssh1#ssh{keyex_key = {{MyPublic,MyPrivate},Curve},
                                             shared_secret = ssh_bits:mpint(K),
                                             exchanged_hash = H,
                                             session_id = sid(Ssh1, H)}};
                {error,unsupported_sign_alg} ->
                    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                io_lib:format("Unsupported algorithm ~p", [SignAlg])
                               )
            end
    catch
        Class:Error ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("ECDH compute key failed in server: ~p:~p~n"
                                      "Kex: ~p, Curve: ~p~n"
                                      "PeerPublic: ~p",
                                      [Class,Error,Kex,Curve,PeerPublic])
                       )
    end.

handle_kex_ecdh_reply(#ssh_msg_kex_ecdh_reply{public_host_key = PeerPubHostKey,
					      q_s = PeerPublic,
					      h_sig = H_SIG},
		      #ssh{keyex_key = {{MyPublic,MyPrivate}, Curve}
                          } = Ssh0
		     ) ->
    %% at client
    try
	compute_key(ecdh, PeerPublic, MyPrivate, Curve)
    of
	K ->
            H = kex_hash(Ssh0, PeerPubHostKey, sha(Curve), {MyPublic,PeerPublic,K}),
	    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
		ok ->
		    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
		    {ok, SshPacket, install_alg(snd, Ssh#ssh{shared_secret  = ssh_bits:mpint(K),
                                                             exchanged_hash = H,
                                                             session_id = sid(Ssh, H)})};
		Error ->
                    ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                io_lib:format("ECDH reply failed. Verify host key: ~p",[Error])
                               )
	    end
    catch
        Class:Error ->
            ?DISCONNECT(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                        io_lib:format("Peer ECDH public key seem invalid: ~p:~p",
                                      [Class,Error])
                       )
    end.


%%%----------------------------------------------------------------
handle_new_keys(#ssh_msg_newkeys{}, Ssh0) ->
    try install_alg(rcv, Ssh0) of
	#ssh{} = Ssh ->
	    {ok, Ssh}
    catch 
        Class:Error -> %% TODO: Throw earlier ...
            ?DISCONNECT(?SSH_DISCONNECT_PROTOCOL_ERROR,
                        io_lib:format("Install alg failed: ~p:~p",
                                      [Class,Error])
                       )
    end. 


%%%----------------------------------------------------------------
kex_ext_info(Role, Opts) ->
    case ?GET_OPT(recv_ext_info,Opts) of
        true when Role==client -> ["ext-info-c"];
        true when Role==server -> ["ext-info-s"];
        false -> []
    end.
    
ext_info_message(#ssh{role=client,
                      send_ext_info=true,
                      opts=Opts} = Ssh0) ->
    %% Since no extension sent by the client is implemented, we add a fake one
    %% to be able to test the framework.
    %% Remove this when there is one and update ssh_protocol_SUITE whare it is used.
    case proplists:get_value(ext_info_client, ?GET_OPT(tstflg,Opts)) of
        true ->
            Msg = #ssh_msg_ext_info{nr_extensions = 1,
                                    data = [{"test@erlang.org", "Testing,PleaseIgnore"}]
                                   },
            {SshPacket, Ssh} = ssh_packet(Msg, Ssh0),
            {ok, SshPacket, Ssh};
        _ ->
            {ok, "", Ssh0}
    end;

ext_info_message(#ssh{role=server,
                      send_ext_info=true,
                      opts = Opts} = Ssh0) ->
    AlgsList = lists:map(fun erlang:atom_to_list/1,
                         ?GET_OPT(pref_public_key_algs, Opts)),
    Msg = #ssh_msg_ext_info{nr_extensions = 1,
                            data = [{"server-sig-algs", string:join(AlgsList,",")}]
                           },
    {SshPacket, Ssh} = ssh_packet(Msg, Ssh0),
    {ok, SshPacket, Ssh};

ext_info_message(Ssh0) ->
    {ok, "", Ssh0}.                          % "" means: 'do not send'

%%%----------------------------------------------------------------
%% select session id
sid(#ssh{session_id = undefined}, H) ->  H;
sid(#ssh{session_id = Id},        _) -> Id.

%%
%% The host key should be read from storage
%%
get_host_key(SignAlg, Opts) ->
    case call_KeyCb(host_key, [SignAlg], Opts) of
	{ok, PrivHostKey} ->
            %% Check the key - the KeyCb may be a buggy plugin
            case valid_key_sha_alg(private, PrivHostKey, SignAlg) of
                true -> PrivHostKey;
                false -> exit({error, bad_hostkey})
            end;
	Result ->
            exit({error, {Result, unsupported_key_type}})
    end.

call_KeyCb(F, Args, Opts) ->
    {KeyCb,KeyCbOpts} = ?GET_OPT(key_cb, Opts),
    UserOpts = ?GET_OPT(key_cb_options, Opts),
    apply(KeyCb, F, Args ++ [[{key_cb_private,KeyCbOpts}|UserOpts]]).


verify_host_key(#ssh{algorithms=Alg}=SSH, PublicKey, Digest, {AlgStr,Signature}) ->
    case atom_to_list(Alg#alg.hkey) of
        AlgStr ->
            case verify(Digest, Alg#alg.hkey, Signature, PublicKey, SSH) of
                false ->
                    {error, bad_signature};
                true ->
                    known_host_key(SSH, PublicKey, public_algo(PublicKey))
            end;
        _ ->
            {error, bad_signature_name}
    end.


%%% -> boolean() | {error,_}
accepted_host(Ssh, PeerName, Port, Public, Opts) ->
    PortStr = case Port of
                  22 -> "";
                  _ -> lists:concat([":",Port])
              end,
    case ?GET_OPT(silently_accept_hosts, Opts) of
        %% Original option values; User question and no host key fingerprints known.
        %% Keep the original question unchanged:
	false -> yes == yes_no(Ssh, "New host " ++ PeerName ++ PortStr ++ " accept");
	true -> true;

        %% Variant: User question but with host key fingerprint in the question:
        {false,Alg} ->
            HostKeyAlg = (Ssh#ssh.algorithms)#alg.hkey,
            Prompt = io_lib:format("The authenticity of the host can't be established.~n"
                                   "~s host key fingerprint is ~s.~n"
                                   "New host ~p~p accept",
                                   [fmt_hostkey(HostKeyAlg),
                                    ssh:hostkey_fingerprint(Alg,Public),
                                    PeerName, PortStr]),
            yes == yes_no(Ssh, Prompt);

        %% Call-back alternatives: A user provided fun is called for the decision:
        F when is_function(F,2) ->
            case catch F(PeerName, ssh:hostkey_fingerprint(Public)) of
                true -> true;
                _ -> {error, fingerprint_check_failed}
            end;

        F when is_function(F,3) ->
            case catch F(PeerName, Port, ssh:hostkey_fingerprint(Public)) of
                true -> true;
                _ -> {error, fingerprint_check_failed}
            end;

	{DigestAlg,F} when is_function(F,2) ->
            case catch F(PeerName, ssh:hostkey_fingerprint(DigestAlg,Public)) of
                true -> true;
                _ -> {error, {fingerprint_check_failed,DigestAlg}}
            end;

	{DigestAlg,F} when is_function(F,3) ->
            case catch F(PeerName, Port, ssh:hostkey_fingerprint(DigestAlg,Public)) of
                true -> true;
                _ -> {error, {fingerprint_check_failed,DigestAlg}}
            end
    end.


yes_no(#ssh{opts=Opts}, Prompt)  ->
    IoCb = ?GET_INTERNAL_OPT(io_cb, Opts, ssh_io),
    IoCb:yes_no(Prompt, Opts).


fmt_hostkey('ssh-rsa') -> "RSA";
fmt_hostkey('ssh-dss') -> "DSA";
fmt_hostkey('ssh-ed25519') -> "ED25519";
fmt_hostkey('ssh-ed448') -> "ED448";
fmt_hostkey(A) when is_atom(A) -> fmt_hostkey(atom_to_list(A));
fmt_hostkey("ecdsa"++_) -> "ECDSA";
fmt_hostkey(X) -> X.


known_host_key(#ssh{opts = Opts, peer = {PeerName,{IP,Port}}} = Ssh, 
	       Public, Alg) ->
    IsHostKey =
        try
            %% New style (with Port)
            call_KeyCb(is_host_key, [Public, [PeerName,IP], Port, Alg], Opts)
        catch
            error:undef ->
                %% old style (without Port)
                call_KeyCb(is_host_key, [Public, PeerName, Alg], Opts)
        end,

    case IsHostKey of
	true ->
	    ok;
	false ->
            %% Not in "known_hosts" and, if is_host_key/4, not revoked
            DoAdd = ?GET_OPT(save_accepted_host, Opts),
	    case accepted_host(Ssh, PeerName, Port, Public, Opts) of
		true when DoAdd == true ->
		    try
                        %% New style (with Port)
                        call_KeyCb(add_host_key, [[PeerName,IP], Port, Public], Opts)
                    catch
                        error:undef ->
                            %% old style (without Port)
                            call_KeyCb(add_host_key, [PeerName, Public], Opts)
                    end;
		true when DoAdd == false ->
                    ok;
		false ->
		    {error, rejected_by_user};
                {error,E} ->
                    {error,E}
	    end;
        {error, Error} ->
            %% Only returned by is_host_key/4
            {error, Error}
    end.

%%   Each of the algorithm strings MUST be a comma-separated list of
%%   algorithm names (see ''Algorithm Naming'' in [SSH-ARCH]).  Each
%%   supported (allowed) algorithm MUST be listed in order of preference.
%%
%%   The first algorithm in each list MUST be the preferred (guessed)
%%   algorithm.  Each string MUST contain at least one algorithm name.
select_algorithm(Role, Client, Server, Opts) ->
    {Encrypt0, Decrypt0} = select_encrypt_decrypt(Role, Client, Server),
    {SendMac0, RecvMac0} = select_send_recv_mac(Role, Client, Server),

    {Encrypt, SendMac} = aead_gcm_simultan(Encrypt0, SendMac0),
    {Decrypt, RecvMac} = aead_gcm_simultan(Decrypt0, RecvMac0),

    {Compression, Decompression} = 
	select_compression_decompression(Role, Client, Server),

    C_Lng = select(Client#ssh_msg_kexinit.languages_client_to_server,
		   Server#ssh_msg_kexinit.languages_client_to_server),
    S_Lng = select(Client#ssh_msg_kexinit.languages_server_to_client,
		   Server#ssh_msg_kexinit.languages_server_to_client),
    HKey = select_all(Client#ssh_msg_kexinit.server_host_key_algorithms,
		      Server#ssh_msg_kexinit.server_host_key_algorithms),
    HK = case HKey of
	     [] -> undefined;
	     [HK0|_] -> HK0
	 end,
    %% Fixme verify Kex against HKey list and algorithms
    
    Kex = select(Client#ssh_msg_kexinit.kex_algorithms,
		 Server#ssh_msg_kexinit.kex_algorithms),

    SendExtInfo =
        %% To send we must have that option enabled and ...
        ?GET_OPT(send_ext_info,Opts) andalso
        %% ... the peer must have told us to send:
        case Role of
            server -> lists:member("ext-info-c", Client#ssh_msg_kexinit.kex_algorithms);
            client -> lists:member("ext-info-s", Server#ssh_msg_kexinit.kex_algorithms)
        end,

    RecvExtInfo =
        %% The peer should not send unless told so by us (which is
        %% guided by an option).
        %% (However a malicious peer could send anyway, so we must be prepared)
        ?GET_OPT(recv_ext_info,Opts),

    {ok, #alg{kex = Kex,
              hkey = HK,
              encrypt = Encrypt,
              decrypt = Decrypt,
              send_mac = SendMac,
              recv_mac = RecvMac,
              compress = Compression,
              decompress = Decompression,
              c_lng = C_Lng,
              s_lng = S_Lng,
              send_ext_info = SendExtInfo,
              recv_ext_info = RecvExtInfo
             }}.


%%% It is an agreed problem with RFC 5674 that if the selection is
%%%   Cipher = AEAD_AES_x_GCM and
%%%      Mac = AEAD_AES_y_GCM (where x =/= y)
%%% then it is undefined what length should be selected.
%%%
%%% If only one of the two lengths (128,256) is available, I claim that
%%% there is no such ambiguity.

%%% From https://anongit.mindrot.org/openssh.git/plain/PROTOCOL
%%% (read Nov 20, 2015)
%%% 1.6 transport: AES-GCM
%%% 
%%% OpenSSH supports the AES-GCM algorithm as specified in RFC 5647.
%%% Because of problems with the specification of the key exchange
%%% the behaviour of OpenSSH differs from the RFC as follows:
%%% 
%%% AES-GCM is only negotiated as the cipher algorithms
%%% "aes128-gcm@openssh.com" or "aes256-gcm@openssh.com" and never as
%%% an MAC algorithm. Additionally, if AES-GCM is selected as the cipher
%%% the exchanged MAC algorithms are ignored and there doesn't have to be
%%% a matching MAC.

aead_gcm_simultan('aes128-gcm@openssh.com', _)         -> {'AEAD_AES_128_GCM', 'AEAD_AES_128_GCM'};
aead_gcm_simultan('aes256-gcm@openssh.com', _)         -> {'AEAD_AES_256_GCM', 'AEAD_AES_256_GCM'};
aead_gcm_simultan('AEAD_AES_128_GCM'=C, _)             -> {C, C};
aead_gcm_simultan('AEAD_AES_256_GCM'=C, _)             -> {C, C};
aead_gcm_simultan(_, 'AEAD_AES_128_GCM'=C)             -> {C, C};
aead_gcm_simultan(_, 'AEAD_AES_256_GCM'=C)             -> {C, C};
aead_gcm_simultan('chacha20-poly1305@openssh.com'=C, _)-> {C, C};
aead_gcm_simultan(Cipher, Mac)                         -> {Cipher,Mac}.


select_encrypt_decrypt(client, Client, Server) ->
    Encrypt = 
	select(Client#ssh_msg_kexinit.encryption_algorithms_client_to_server,
	       Server#ssh_msg_kexinit.encryption_algorithms_client_to_server),
    Decrypt = 
	select(Client#ssh_msg_kexinit.encryption_algorithms_server_to_client,
	       Server#ssh_msg_kexinit.encryption_algorithms_server_to_client),
    {Encrypt, Decrypt};
select_encrypt_decrypt(server, Client, Server) ->
    Decrypt = 
	select(Client#ssh_msg_kexinit.encryption_algorithms_client_to_server,
	       Server#ssh_msg_kexinit.encryption_algorithms_client_to_server),
    Encrypt = 
	select(Client#ssh_msg_kexinit.encryption_algorithms_server_to_client,
	       Server#ssh_msg_kexinit.encryption_algorithms_server_to_client),
    {Encrypt, Decrypt}.

select_send_recv_mac(client, Client, Server) ->
    SendMac = select(Client#ssh_msg_kexinit.mac_algorithms_client_to_server,
		     Server#ssh_msg_kexinit.mac_algorithms_client_to_server),
    RecvMac = select(Client#ssh_msg_kexinit.mac_algorithms_server_to_client,
		     Server#ssh_msg_kexinit.mac_algorithms_server_to_client),
    {SendMac, RecvMac};
select_send_recv_mac(server, Client, Server) ->
    RecvMac = select(Client#ssh_msg_kexinit.mac_algorithms_client_to_server,
		      Server#ssh_msg_kexinit.mac_algorithms_client_to_server),
    SendMac = select(Client#ssh_msg_kexinit.mac_algorithms_server_to_client,
		      Server#ssh_msg_kexinit.mac_algorithms_server_to_client),
    {SendMac, RecvMac}.

select_compression_decompression(client, Client, Server) ->
    Compression = 
	select(Client#ssh_msg_kexinit.compression_algorithms_client_to_server,
	       Server#ssh_msg_kexinit.compression_algorithms_client_to_server),
    Decompression = 
	select(Client#ssh_msg_kexinit.compression_algorithms_server_to_client,
	       Server#ssh_msg_kexinit.compression_algorithms_server_to_client),
    {Compression, Decompression};
select_compression_decompression(server, Client, Server) ->
    Decompression = 
	select(Client#ssh_msg_kexinit.compression_algorithms_client_to_server,
	       Server#ssh_msg_kexinit.compression_algorithms_client_to_server),
    Compression = 
	select(Client#ssh_msg_kexinit.compression_algorithms_server_to_client,
	       Server#ssh_msg_kexinit.compression_algorithms_server_to_client),
    {Compression, Decompression}.

%% DIr = rcv | snd
install_alg(Dir, SSH) ->
    SSH1 = alg_final(Dir, SSH),
    SSH2 = alg_setup(Dir, SSH1),
    alg_init(Dir, SSH2).

alg_setup(snd, SSH) ->
    ALG = SSH#ssh.algorithms,
    SSH#ssh{encrypt = ALG#alg.encrypt,
	    send_mac = ALG#alg.send_mac,
	    send_mac_size = mac_digest_size(ALG#alg.send_mac),
	    compress = ALG#alg.compress,
	    c_lng = ALG#alg.c_lng,
	    s_lng = ALG#alg.s_lng,
            send_ext_info = ALG#alg.send_ext_info,
            recv_ext_info = ALG#alg.recv_ext_info
	   };

alg_setup(rcv, SSH) ->
    ALG = SSH#ssh.algorithms,
    SSH#ssh{decrypt = ALG#alg.decrypt,
	    recv_mac = ALG#alg.recv_mac,
	    recv_mac_size = mac_digest_size(ALG#alg.recv_mac),
	    decompress = ALG#alg.decompress,
	    c_lng = ALG#alg.c_lng,
	    s_lng = ALG#alg.s_lng,
            send_ext_info = ALG#alg.send_ext_info,
            recv_ext_info = ALG#alg.recv_ext_info
	   }.


alg_init(snd, SSH0) ->
    {ok,SSH1} = send_mac_init(SSH0),
    {ok,SSH2} = encrypt_init(SSH1),
    {ok,SSH3} = compress_init(SSH2),
    SSH3;

alg_init(rcv, SSH0) ->
    {ok,SSH1} = recv_mac_init(SSH0),
    {ok,SSH2} = decrypt_init(SSH1),
    {ok,SSH3} = decompress_init(SSH2),
    SSH3.


alg_final(snd, SSH0) ->
    {ok,SSH1} = send_mac_final(SSH0),
    {ok,SSH2} = encrypt_final(SSH1),
    {ok,SSH3} = compress_final(SSH2),
    SSH3;

alg_final(rcv, SSH0) ->
    {ok,SSH1} = recv_mac_final(SSH0),
    {ok,SSH2} = decrypt_final(SSH1),
    {ok,SSH3} = decompress_final(SSH2),
    SSH3.


select_all(CL, SL) when length(CL) + length(SL) < ?MAX_NUM_ALGORITHMS ->
    %% algorithms only used by client
    %% NOTE: an algorithm occurring more than once in CL will still be present
    %%       in CLonly. This is not a problem for nice clients.
    CLonly = CL -- SL,

    %% algorithms used by client and server (client pref)
    lists:foldr(fun(ALG, Acc) -> 
                      try [list_to_existing_atom(ALG) | Acc]
                      catch
                          %% If an malicious client uses the same non-existing algorithm twice,
                          %% we will end up here
                          _:_ -> Acc
                      end
              end, [], (CL -- CLonly));

select_all(CL, SL) ->
    Error = lists:concat(["Received too many algorithms (",length(CL),"+",length(SL)," >= ",?MAX_NUM_ALGORITHMS,")."]),
    ?DISCONNECT(?SSH_DISCONNECT_PROTOCOL_ERROR,
                Error).


select([], []) ->
    none;
select(CL, SL) ->
    C = case select_all(CL,SL) of
	    [] -> undefined;
	    [ALG|_] -> ALG
	end,
    C.
	    
ssh_packet(#ssh_msg_kexinit{} = Msg, Ssh0) ->
    BinMsg = ssh_message:encode(Msg),
    Ssh = key_init(Ssh0#ssh.role, Ssh0, BinMsg),
    pack(BinMsg, Ssh);

ssh_packet(Msg, Ssh) ->
    BinMsg = ssh_message:encode(Msg),
    pack(BinMsg, Ssh).

pack(Data, Ssh=#ssh{}) ->
    pack(Data, Ssh, 0).

%%% Note: pack/3 is only to be called from tests that wants
%%% to deliberetly send packets with wrong PacketLength!
%%% Use pack/2 for all other purposes!
pack(PlainText,
     #ssh{send_sequence = SeqNum,
	  send_mac = MacAlg,
	  encrypt = CryptoAlg} = Ssh0,  PacketLenDeviationForTests) when is_binary(PlainText) ->
    {Ssh1, CompressedPlainText} = compress(Ssh0, PlainText),
    {FinalPacket, Ssh2} = pack(pkt_type(CryptoAlg), mac_type(MacAlg), 
                               CompressedPlainText, PacketLenDeviationForTests,
                               Ssh1),
    Ssh = Ssh2#ssh{send_sequence = (SeqNum+1) band 16#ffffffff},
    {FinalPacket, Ssh}.


pack(common, rfc4253, PlainText, DeltaLenTst,
     #ssh{send_sequence = SeqNum,
          send_mac = MacAlg,
          send_mac_key = MacKey} = Ssh0) ->
    PadLen = padding_length(4+1+byte_size(PlainText), Ssh0),
    Pad =  ssh_bits:random(PadLen),
    TextLen = 1 + byte_size(PlainText) + PadLen + DeltaLenTst,
    PlainPkt = <<?UINT32(TextLen),?BYTE(PadLen), PlainText/binary, Pad/binary>>,
    {Ssh1, CipherPkt} = encrypt(Ssh0, PlainPkt),
    MAC0 = mac(MacAlg, MacKey, SeqNum, PlainPkt),
    {<<CipherPkt/binary,MAC0/binary>>, Ssh1};


pack(common, enc_then_mac, PlainText, DeltaLenTst,
     #ssh{send_sequence = SeqNum,
          send_mac = MacAlg,
          send_mac_key = MacKey} = Ssh0) ->
    PadLen = padding_length(1+byte_size(PlainText), Ssh0),
    Pad =  ssh_bits:random(PadLen),
    PlainLen = 1 + byte_size(PlainText) + PadLen + DeltaLenTst,
    PlainPkt = <<?BYTE(PadLen), PlainText/binary, Pad/binary>>,
    {Ssh1, CipherPkt} = encrypt(Ssh0, PlainPkt),
    EncPacketPkt = <<?UINT32(PlainLen), CipherPkt/binary>>,
    MAC0 = mac(MacAlg, MacKey, SeqNum, EncPacketPkt),
    {<<?UINT32(PlainLen), CipherPkt/binary, MAC0/binary>>, Ssh1};

pack(aead, _, PlainText, DeltaLenTst, Ssh0) ->
    PadLen = padding_length(1+byte_size(PlainText), Ssh0),
    Pad =  ssh_bits:random(PadLen),
    PlainLen = 1 + byte_size(PlainText) + PadLen + DeltaLenTst,
    PlainPkt = <<?BYTE(PadLen), PlainText/binary, Pad/binary>>,
    {Ssh1, {CipherPkt,MAC0}} = encrypt(Ssh0, <<?UINT32(PlainLen),PlainPkt/binary>>),
    {<<CipherPkt/binary,MAC0/binary>>, Ssh1}.

%%%================================================================
handle_packet_part(<<>>, Encrypted0, AEAD0, undefined, #ssh{decrypt = CryptoAlg,
                                                            recv_mac = MacAlg} = Ssh0) ->
    %% New ssh packet
    case get_length(pkt_type(CryptoAlg), mac_type(MacAlg), Encrypted0, Ssh0) of
	get_more ->
	    %% too short to get the length
	    {get_more, <<>>, Encrypted0, AEAD0, undefined, Ssh0};

	{ok, PacketLen, _, _, _, _} when PacketLen > ?SSH_MAX_PACKET_SIZE ->
	    %% far too long message than expected
	    {error, {exceeds_max_size,PacketLen}};
	
	{ok, PacketLen, Decrypted, Encrypted1, AEAD,
	 #ssh{recv_mac_size = MacSize} = Ssh1} ->
	    %% enough bytes so we got the length and can calculate how many
	    %% more bytes to expect for a full packet
	    TotalNeeded = (4 + PacketLen + MacSize),
	    handle_packet_part(Decrypted, Encrypted1, AEAD, TotalNeeded, Ssh1)
    end;

handle_packet_part(DecryptedPfx, EncryptedBuffer, AEAD, TotalNeeded, Ssh0) 
  when (byte_size(DecryptedPfx)+byte_size(EncryptedBuffer)) < TotalNeeded ->
    %% need more bytes to finalize the packet
    {get_more, DecryptedPfx, EncryptedBuffer, AEAD, TotalNeeded, Ssh0};

handle_packet_part(DecryptedPfx, EncryptedBuffer, AEAD, TotalNeeded, #ssh{decrypt = CryptoAlg,
                                                                          recv_mac = MacAlg} = Ssh0) ->
    %% enough bytes to decode the packet.
    case unpack(pkt_type(CryptoAlg), mac_type(MacAlg),
                DecryptedPfx, EncryptedBuffer, AEAD, TotalNeeded, Ssh0) of
        {ok, Payload, NextPacketBytes, Ssh1} ->
            {Ssh, DecompressedPayload} = decompress(Ssh1, Payload),
            {packet_decrypted, DecompressedPayload, NextPacketBytes, Ssh};
        Other ->
            Other
    end.

%%%----------------
unpack(common, rfc4253, DecryptedPfx, EncryptedBuffer, _AEAD, TotalNeeded,
       #ssh{recv_mac_size = MacSize} = Ssh0) ->
    MoreNeeded = TotalNeeded - byte_size(DecryptedPfx) - MacSize,
    <<EncryptedSfx:MoreNeeded/binary, Mac:MacSize/binary, NextPacketBytes/binary>> = EncryptedBuffer,
    {Ssh1, DecryptedSfx} = decrypt(Ssh0, EncryptedSfx),
    PlainPkt = <<DecryptedPfx/binary, DecryptedSfx/binary>>,
    case is_valid_mac(Mac, PlainPkt, Ssh1) of
        true ->
            {ok, payload(PlainPkt), NextPacketBytes, Ssh1};
        false ->
            {bad_mac, Ssh1}
    end;

unpack(common, enc_then_mac, <<?UINT32(PlainLen)>>, EncryptedBuffer, _AEAD, _TotalNeeded,
       #ssh{recv_mac_size = MacSize} = Ssh0) ->
    <<Payload:PlainLen/binary, MAC0:MacSize/binary, NextPacketBytes/binary>> = EncryptedBuffer,
    case is_valid_mac(MAC0, <<?UINT32(PlainLen),Payload/binary>>, Ssh0) of
        true ->
            {Ssh1, <<?BYTE(PaddingLen), PlainRest/binary>>} = decrypt(Ssh0, Payload),
            CompressedPlainTextLen = byte_size(PlainRest) - PaddingLen,
            <<CompressedPlainText:CompressedPlainTextLen/binary, _Padding/binary>> = PlainRest,
            {ok, CompressedPlainText, NextPacketBytes, Ssh1};
        false ->
            {bad_mac, Ssh0}
    end;
                    
unpack(aead, _, DecryptedPfx, EncryptedBuffer, AEAD, TotalNeeded, 
       #ssh{recv_mac_size = MacSize} = Ssh0) ->
    %% enough bytes to decode the packet.
    MoreNeeded = TotalNeeded - byte_size(DecryptedPfx) - MacSize,
    <<EncryptedSfx:MoreNeeded/binary, Mac:MacSize/binary, NextPacketBytes/binary>> = EncryptedBuffer,
    case decrypt(Ssh0, {AEAD,EncryptedSfx,Mac}) of
        {Ssh1, error} ->
            {bad_mac, Ssh1};
        {Ssh1, DecryptedSfx} ->
            DecryptedPacket = <<DecryptedPfx/binary, DecryptedSfx/binary>>,
            {ok, payload(DecryptedPacket), NextPacketBytes, Ssh1}
    end.

%%%----------------------------------------------------------------
get_length(common, rfc4253, EncryptedBuffer, #ssh{decrypt_block_size = BlockSize} = Ssh0) ->
    case byte_size(EncryptedBuffer) >= erlang:max(8, BlockSize) of
	true ->
	    <<EncBlock:BlockSize/binary, EncryptedRest/binary>> = EncryptedBuffer,
	    {Ssh, 
	     <<?UINT32(PacketLen),_/binary>> = Decrypted} = decrypt(Ssh0, EncBlock),
	    {ok, PacketLen, Decrypted, EncryptedRest, <<>>, Ssh};
	false ->
	    get_more
    end;

get_length(common, enc_then_mac, EncryptedBuffer, Ssh) ->
    case EncryptedBuffer of
        <<Decrypted:4/binary, EncryptedRest/binary>> ->  
            <<?UINT32(PacketLen)>> = Decrypted,
            {ok, PacketLen, Decrypted, EncryptedRest, <<>>, Ssh};
        _ ->
            get_more
    end;

get_length(aead, _, EncryptedBuffer, Ssh) ->
    case {byte_size(EncryptedBuffer) >= 4, Ssh#ssh.decrypt} of
       {true, 'chacha20-poly1305@openssh.com'} ->
            <<EncryptedLen:4/binary, EncryptedRest/binary>> = EncryptedBuffer,
            {Ssh1,  PacketLenBin} = decrypt(Ssh, {length,EncryptedLen}),
            <<?UINT32(PacketLen)>> = PacketLenBin,
            {ok, PacketLen, PacketLenBin, EncryptedRest, EncryptedLen, Ssh1};
        {true, _} ->
	    <<?UINT32(PacketLen), EncryptedRest/binary>> = EncryptedBuffer,
            {ok, PacketLen, <<?UINT32(PacketLen)>>, EncryptedRest, <<?UINT32(PacketLen)>>, Ssh};
        {false, _} ->
	    get_more
    end.


padding_length(Size, #ssh{encrypt_block_size = BlockSize,
			  random_length_padding = RandomLengthPad}) ->
    PL = (BlockSize - (Size rem BlockSize)) rem BlockSize,
    MinPadLen = if PL <  4 -> PL + BlockSize;
		       true -> PL
		    end,
    PadBlockSize =  max(BlockSize,4),
    MaxExtraBlocks = (max(RandomLengthPad,MinPadLen) - MinPadLen) div PadBlockSize,
    ExtraPadLen = try (rand:uniform(MaxExtraBlocks+1) - 1) * PadBlockSize
		      catch _:_ -> 0
		      end,
    MinPadLen + ExtraPadLen.


payload(<<PacketLen:32, PaddingLen:8, PayloadAndPadding/binary>>) ->
    PayloadLen = PacketLen - PaddingLen - 1,
    <<Payload:PayloadLen/binary, _/binary>> = PayloadAndPadding,
    Payload.

%%%----------------------------------------------------------------
%% sign(SigData, SignAlg, Key, Opts) when is_list(SignAlg) ->
%%     sign(SigData, list_to_existing_atom(SignAlg), Key, Opts);

sign(SigData, SignAlg, Key, #ssh{opts=Opts}) when is_atom(SignAlg) ->
    case lists:member(SignAlg,
                      proplists:get_value(public_key,
                                          ?GET_OPT(preferred_algorithms,Opts,[]))) of
        true ->
            {ok, sign(SigData, sha(SignAlg), Key)};
        false ->
            {error, unsupported_sign_alg}
    end.

sign(SigData, HashAlg, #{algorithm:=dss} = Key) ->
    mk_dss_sig(crypto:sign(dss, HashAlg, SigData, Key));
sign(SigData, HashAlg, #{algorithm:=SigAlg} = Key) ->
    crypto:sign(SigAlg, HashAlg, SigData, Key);
sign(SigData, HashAlg,  #'DSAPrivateKey'{} = Key) ->
    mk_dss_sig(public_key:sign(SigData, HashAlg, Key));
sign(SigData, HashAlg, Key = #'ECPrivateKey'{parameters = {namedCurve, Curve}})
  when (Curve == ?'id-Ed25519') orelse (Curve == ?'id-Ed448') ->
    public_key:sign(SigData, HashAlg, Key);
sign(SigData, HashAlg, Key = #'ECPrivateKey'{}) ->
    DerEncodedSign =  public_key:sign(SigData, HashAlg, Key),
    #'ECDSA-Sig-Value'{r=R, s=S} = public_key:der_decode('ECDSA-Sig-Value', DerEncodedSign),
    <<?Empint(R),?Empint(S)>>;
sign(SigData, HashAlg, Key) ->
    public_key:sign(SigData, HashAlg, Key).


mk_dss_sig(DerSignature) ->
    #'Dss-Sig-Value'{r = R, s = S} = public_key:der_decode('Dss-Sig-Value', DerSignature),
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>>.

%%%----------------------------------------------------------------
verify(PlainText, Alg, Sig, Key, Ssh) ->
    do_verify(PlainText, sha(Alg), Sig, Key, Ssh).


do_verify(PlainText, HashAlg, Sig, {_,  #'Dss-Parms'{}} = Key, _) ->
    case Sig of
        <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>> ->
            Signature = public_key:der_encode('Dss-Sig-Value', #'Dss-Sig-Value'{r = R, s = S}),
            public_key:verify(PlainText, HashAlg, Signature, Key);
        _ ->
            false
    end;
do_verify(PlainText, HashAlg, Sig, {#'ECPoint'{},_} = Key, _) when HashAlg =/= undefined ->
    case Sig of
        <<?UINT32(Rlen),R:Rlen/big-signed-integer-unit:8,
          ?UINT32(Slen),S:Slen/big-signed-integer-unit:8>> ->
            Sval = #'ECDSA-Sig-Value'{r=R, s=S},
            DerEncodedSig = public_key:der_encode('ECDSA-Sig-Value',Sval),
            public_key:verify(PlainText, HashAlg, DerEncodedSig, Key);
        _ ->
            false
    end;

do_verify(PlainText, HashAlg, Sig, #'RSAPublicKey'{}=Key, #ssh{role = server,
                                                               c_version = "SSH-2.0-OpenSSH_7."++_})
  when HashAlg == sha256; HashAlg == sha512 ->
    %% Public key signing bug in in OpenSSH >= 7.2
    public_key:verify(PlainText, HashAlg, Sig, Key)
        orelse public_key:verify(PlainText, sha, Sig, Key);

do_verify(PlainText, HashAlg, Sig, Key, _) ->
    public_key:verify(PlainText, HashAlg, Sig, Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Encryption
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Unit: bytes

-record(cipher, {
                 impl,
                 key_bytes,
                 iv_bytes,
                 block_bytes,
                 pkt_type = common
                }).

%%% Start of a more parameterized crypto handling.
cipher('AEAD_AES_128_GCM') ->
    #cipher{impl = aes_128_gcm,
            key_bytes = 16,
            iv_bytes = 12,
            block_bytes = 16,
            pkt_type = aead};

cipher('AEAD_AES_256_GCM') ->
    #cipher{impl = aes_256_gcm,
            key_bytes = 32,
            iv_bytes = 12,
            block_bytes = 16,
            pkt_type = aead};

cipher('3des-cbc') ->
    #cipher{impl = des_ede3_cbc,
            key_bytes = 24,
            iv_bytes = 8,
            block_bytes = 8};
    
cipher('aes128-cbc') ->
    #cipher{impl = aes_128_cbc,
            key_bytes = 16,
            iv_bytes = 16,
            block_bytes = 16};

cipher('aes192-cbc') ->
    #cipher{impl = aes_192_cbc,
            key_bytes = 24,
            iv_bytes = 16,
            block_bytes = 16};

cipher('aes256-cbc') ->
    #cipher{impl = aes_256_cbc,
            key_bytes = 32,
            iv_bytes = 16,
            block_bytes = 16};

cipher('aes128-ctr') ->
    #cipher{impl = aes_128_ctr,
            key_bytes = 16,
            iv_bytes = 16,
            block_bytes = 16};

cipher('aes192-ctr') ->
    #cipher{impl = aes_192_ctr,
            key_bytes = 24,
            iv_bytes = 16,
            block_bytes = 16};

cipher('aes256-ctr') ->
    #cipher{impl = aes_256_ctr,
            key_bytes = 32,
            iv_bytes = 16,
            block_bytes = 16};

cipher('chacha20-poly1305@openssh.com') -> % FIXME: Verify!!
    #cipher{impl = chacha20_poly1305,
            key_bytes = 32,
            iv_bytes = 12,
            block_bytes = 8,
            pkt_type = aead};

cipher(_) -> 
    #cipher{}.


pkt_type(SshCipher) -> (cipher(SshCipher))#cipher.pkt_type.

mac_type('hmac-sha2-256-etm@openssh.com') -> enc_then_mac;
mac_type('hmac-sha2-512-etm@openssh.com') -> enc_then_mac;
mac_type('hmac-sha1-etm@openssh.com') -> enc_then_mac;
mac_type(_) -> rfc4253.
    
decrypt_magic(server) -> {"A", "C"};
decrypt_magic(client) -> {"B", "D"}.

encrypt_magic(client) -> decrypt_magic(server);
encrypt_magic(server) -> decrypt_magic(client).



encrypt_init(#ssh{encrypt = none} = Ssh) ->
    {ok, Ssh};

encrypt_init(#ssh{encrypt = 'chacha20-poly1305@openssh.com', role = Role} = Ssh) ->
    %% chacha20-poly1305@openssh.com uses two independent crypto streams, one (chacha20)
    %% for the length used in stream mode, and the other (chacha20-poly1305) as AEAD for
    %% the payload and to MAC the length||payload.
    %% See draft-josefsson-ssh-chacha20-poly1305-openssh-00
    {_, KeyMagic} = encrypt_magic(Role),
    <<K2:32/binary,K1:32/binary>> = hash(Ssh, KeyMagic, 8*64),
    {ok, Ssh#ssh{encrypt_keys = {K1,K2}
                % encrypt_block_size = 16, %default = 8.  What to set it to? 64 (openssl chacha.h)
                 % ctx and iv is setup for each packet
                }};

encrypt_init(#ssh{encrypt = SshCipher, role = Role} = Ssh) when SshCipher == 'AEAD_AES_128_GCM';
                                                                SshCipher == 'AEAD_AES_256_GCM' ->
    {IvMagic, KeyMagic} = encrypt_magic(Role),
    #cipher{impl = CryptoCipher,
            key_bytes = KeyBytes,
            iv_bytes = IvBytes,
            block_bytes = BlockBytes} = cipher(SshCipher),
    IV = hash(Ssh, IvMagic, 8*IvBytes),
    K = hash(Ssh, KeyMagic, 8*KeyBytes),
    {ok, Ssh#ssh{encrypt_cipher = CryptoCipher,
                 encrypt_keys = K,
		 encrypt_block_size = BlockBytes,
		 encrypt_ctx = IV}};

encrypt_init(#ssh{encrypt = SshCipher, role = Role} = Ssh) ->
    {IvMagic, KeyMagic} = encrypt_magic(Role),
    #cipher{impl = CryptoCipher,
            key_bytes = KeyBytes,
            iv_bytes = IvBytes,
            block_bytes = BlockBytes} = cipher(SshCipher),
    IV = hash(Ssh, IvMagic, 8*IvBytes),
    K = hash(Ssh, KeyMagic, 8*KeyBytes),
    Ctx0 = crypto:crypto_init(CryptoCipher, K, IV, true),
    {ok, Ssh#ssh{encrypt_cipher = CryptoCipher,
                 encrypt_block_size = BlockBytes,
                 encrypt_ctx = Ctx0}}.

encrypt_final(Ssh) ->
    {ok, Ssh#ssh{encrypt = none,
		 encrypt_keys = undefined,
		 encrypt_block_size = 8,
		 encrypt_ctx = undefined
		}}.


encrypt(#ssh{encrypt = none} = Ssh, Data) ->
    {Ssh, Data};

encrypt(#ssh{encrypt = 'chacha20-poly1305@openssh.com',
             encrypt_keys = {K1,K2},
             send_sequence = Seq} = Ssh,
        <<LenData:4/binary, PayloadData/binary>>) ->
    %% Encrypt length
    IV1 = <<0:8/unit:8, Seq:8/unit:8>>,
    EncLen = crypto:crypto_one_time(chacha20, K1, IV1, LenData, true),
    %% Encrypt payload
    IV2 = <<1:8/little-unit:8, Seq:8/unit:8>>,
    EncPayloadData = crypto:crypto_one_time(chacha20, K2, IV2, PayloadData, true),
    %% MAC tag
    PolyKey = crypto:crypto_one_time(chacha20, K2, <<0:8/unit:8,Seq:8/unit:8>>, <<0:32/unit:8>>, true),
    EncBytes = <<EncLen/binary,EncPayloadData/binary>>,
    Ctag = crypto:mac(poly1305, PolyKey, EncBytes),
    %% Result
    {Ssh, {EncBytes,Ctag}};

encrypt(#ssh{encrypt = SshCipher,
             encrypt_cipher = CryptoCipher,
             encrypt_keys = K,
             encrypt_ctx = IV0} = Ssh,
        <<LenData:4/binary, PayloadData/binary>>) when SshCipher == 'AEAD_AES_128_GCM' ;
                                                       SshCipher == 'AEAD_AES_256_GCM' ->
    {Ctext,Ctag} = crypto:crypto_one_time_aead(CryptoCipher, K, IV0, PayloadData, LenData, true),
    IV = next_gcm_iv(IV0),
    {Ssh#ssh{encrypt_ctx = IV}, {<<LenData/binary,Ctext/binary>>,Ctag}};

encrypt(#ssh{encrypt_ctx = Ctx0} = Ssh, Data) ->
    Enc = crypto:crypto_update(Ctx0, Data),
    {Ssh, Enc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decryption
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decrypt_init(#ssh{decrypt = none} = Ssh) ->
    {ok, Ssh};

decrypt_init(#ssh{decrypt = 'chacha20-poly1305@openssh.com', role = Role} = Ssh) ->
    {_, KeyMagic} = decrypt_magic(Role),
    <<K2:32/binary,K1:32/binary>> = hash(Ssh, KeyMagic, 8*64),
    {ok, Ssh#ssh{decrypt_keys = {K1,K2}
                }};

decrypt_init(#ssh{decrypt = SshCipher, role = Role} = Ssh) when SshCipher == 'AEAD_AES_128_GCM';
                                                                SshCipher == 'AEAD_AES_256_GCM' ->
    {IvMagic, KeyMagic} = decrypt_magic(Role),
    #cipher{impl = CryptoCipher,
            key_bytes = KeyBytes,
            iv_bytes = IvBytes,
            block_bytes = BlockBytes} = cipher(SshCipher),
    IV = hash(Ssh, IvMagic, 8*IvBytes),
    K = hash(Ssh, KeyMagic, 8*KeyBytes),
    {ok, Ssh#ssh{decrypt_cipher = CryptoCipher,
                 decrypt_keys = K,
		 decrypt_block_size = BlockBytes,
		 decrypt_ctx = IV}};

decrypt_init(#ssh{decrypt = SshCipher, role = Role} = Ssh) ->
    {IvMagic, KeyMagic} = decrypt_magic(Role),
    #cipher{impl = CryptoCipher,
            key_bytes = KeyBytes,
            iv_bytes = IvBytes,
            block_bytes = BlockBytes} = cipher(SshCipher),
    IV = hash(Ssh, IvMagic, 8*IvBytes),
    K = hash(Ssh, KeyMagic, 8*KeyBytes),
    Ctx0 = crypto:crypto_init(CryptoCipher, K, IV, false),
    {ok, Ssh#ssh{decrypt_cipher = CryptoCipher,
                 decrypt_block_size = BlockBytes,
                 decrypt_ctx = Ctx0}}.


decrypt_final(Ssh) ->
    {ok, Ssh#ssh {decrypt = none, 
		  decrypt_keys = undefined,
		  decrypt_ctx = undefined,
		  decrypt_block_size = 8}}.


decrypt(Ssh, <<>>) ->
    {Ssh, <<>>};

decrypt(#ssh{decrypt = 'chacha20-poly1305@openssh.com',
             decrypt_keys = {K1,K2},
             recv_sequence = Seq} = Ssh, Data) ->
    case Data of
        {length,EncryptedLen} ->
            %% The length is decrypted separately in a first step
            PacketLenBin = crypto:crypto_one_time(chacha20, K1, <<0:8/unit:8, Seq:8/unit:8>>, EncryptedLen, false),
            {Ssh, PacketLenBin};
         {AAD,Ctext,Ctag} ->
            %% The length is already decrypted and used to divide the input
            %% Check the mac (important that it is timing-safe):
            PolyKey = crypto:crypto_one_time(chacha20, K2, <<0:8/unit:8,Seq:8/unit:8>>, <<0:32/unit:8>>, false),
            case ssh_lib:comp(Ctag, crypto:mac(poly1305, PolyKey, <<AAD/binary,Ctext/binary>>)) of
                true ->
                    %% MAC is ok, decode
                    IV2 = <<1:8/little-unit:8, Seq:8/unit:8>>,
                    PlainText = crypto:crypto_one_time(chacha20, K2, IV2, Ctext, false),
                    {Ssh, PlainText};
                false ->
                    {Ssh,error}
            end
    end;

decrypt(#ssh{decrypt = none} = Ssh, Data) ->
    {Ssh, Data};

decrypt(#ssh{decrypt = SshCipher,
             decrypt_cipher = CryptoCipher,
	     decrypt_keys = K,
	     decrypt_ctx = IV0} = Ssh, {AAD,Ctext,Ctag}) when SshCipher == 'AEAD_AES_128_GCM' ;
                                                              SshCipher == 'AEAD_AES_256_GCM' ->
    Dec = crypto:crypto_one_time_aead(CryptoCipher, K, IV0, Ctext, AAD, Ctag, false),
    IV = next_gcm_iv(IV0),
    {Ssh#ssh{decrypt_ctx = IV}, Dec};

decrypt(#ssh{decrypt_ctx = Ctx0} = Ssh, Data) ->
    Dec = crypto:crypto_update(Ctx0, Data),
    {Ssh, Dec}.

next_gcm_iv(<<Fixed:32, InvCtr:64>>) -> <<Fixed:32, (InvCtr+1):64>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compression
%%
%%     none             REQUIRED        no compression
%%     zlib             OPTIONAL        ZLIB (LZ77) compression
%%     openssh_zlib     OPTIONAL        ZLIB (LZ77) compression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compress_init(SSH) ->
    compress_init(SSH, 1).

compress_init(#ssh{compress = none} = Ssh, _) ->
    {ok, Ssh};
compress_init(#ssh{compress = zlib} = Ssh, Level) ->
    Zlib = zlib:open(),
    ok = zlib:deflateInit(Zlib, Level),
    {ok, Ssh#ssh{compress_ctx = Zlib}};
compress_init(#ssh{compress = 'zlib@openssh.com'} = Ssh, Level) ->
    Zlib = zlib:open(),
    ok = zlib:deflateInit(Zlib, Level),
    {ok, Ssh#ssh{compress_ctx = Zlib}}.

compress_final(#ssh{compress = none} = Ssh) ->
    {ok, Ssh};
compress_final(#ssh{compress = zlib, compress_ctx = Context} = Ssh) ->
    zlib:close(Context),
    {ok, Ssh#ssh{compress = none, compress_ctx = undefined}};
compress_final(#ssh{compress = 'zlib@openssh.com', authenticated = false} = Ssh) ->
    {ok, Ssh};
compress_final(#ssh{compress = 'zlib@openssh.com', compress_ctx = Context, authenticated = true} = Ssh) ->
    zlib:close(Context),
    {ok, Ssh#ssh{compress = none, compress_ctx = undefined}}.

compress(#ssh{compress = none} = Ssh, Data) ->
    {Ssh, Data};
compress(#ssh{compress = zlib, compress_ctx = Context} = Ssh, Data) ->
    Compressed = zlib:deflate(Context, Data, sync),
    {Ssh, list_to_binary(Compressed)};
compress(#ssh{compress = 'zlib@openssh.com', authenticated = false} = Ssh, Data) ->
    {Ssh, Data};
compress(#ssh{compress = 'zlib@openssh.com', compress_ctx = Context, authenticated = true} = Ssh, Data) ->
    Compressed = zlib:deflate(Context, Data, sync),
    {Ssh, list_to_binary(Compressed)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decompression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompress_init(#ssh{decompress = none} = Ssh) ->
    {ok, Ssh};
decompress_init(#ssh{decompress = zlib} = Ssh) ->
    Zlib = zlib:open(),
    ok = zlib:inflateInit(Zlib),
    {ok, Ssh#ssh{decompress_ctx = Zlib}};
decompress_init(#ssh{decompress = 'zlib@openssh.com'} = Ssh) ->
    Zlib = zlib:open(),
    ok = zlib:inflateInit(Zlib),
    {ok, Ssh#ssh{decompress_ctx = Zlib}}.

decompress_final(#ssh{decompress = none} = Ssh) ->
    {ok, Ssh};
decompress_final(#ssh{decompress = zlib, decompress_ctx = Context} = Ssh) ->
    zlib:close(Context),
    {ok, Ssh#ssh{decompress = none, decompress_ctx = undefined}};
decompress_final(#ssh{decompress = 'zlib@openssh.com', authenticated = false} = Ssh) ->
    {ok, Ssh};
decompress_final(#ssh{decompress = 'zlib@openssh.com', decompress_ctx = Context, authenticated = true} = Ssh) ->
    zlib:close(Context),
    {ok, Ssh#ssh{decompress = none, decompress_ctx = undefined}}.

decompress(#ssh{decompress = none} = Ssh, Data) ->
    {Ssh, Data};
decompress(#ssh{decompress = zlib, decompress_ctx = Context} = Ssh, Data) ->
    Decompressed = zlib:inflate(Context, Data),
    {Ssh, list_to_binary(Decompressed)};
decompress(#ssh{decompress = 'zlib@openssh.com', authenticated = false} = Ssh, Data) ->
    {Ssh, Data};
decompress(#ssh{decompress = 'zlib@openssh.com', decompress_ctx = Context, authenticated = true} = Ssh, Data) ->
    Decompressed = zlib:inflate(Context, Data),
    {Ssh, list_to_binary(Decompressed)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% MAC calculation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_mac_init(SSH) ->
    case pkt_type(SSH#ssh.send_mac) of
	common ->
	    case SSH#ssh.role of
		client ->
		    KeySize = 8*mac_key_bytes(SSH#ssh.send_mac),
		    Key = hash(SSH, "E", KeySize),
		    {ok, SSH#ssh { send_mac_key = Key }};
		server ->
		    KeySize = 8*mac_key_bytes(SSH#ssh.send_mac),
		    Key = hash(SSH, "F", KeySize),
		    {ok, SSH#ssh { send_mac_key = Key }}
	    end;
	_ ->
	    %% Not applicable
	    {ok, SSH}
    end.

send_mac_final(SSH) ->
    {ok, SSH#ssh {send_mac = none, 
		  send_mac_key = undefined }}.


recv_mac_init(SSH) ->
    case pkt_type(SSH#ssh.recv_mac) of
	common ->
	    case SSH#ssh.role of
		client ->
		    Key = hash(SSH, "F", 8*mac_key_bytes(SSH#ssh.recv_mac)),
		    {ok, SSH#ssh { recv_mac_key = Key }};
		server ->
		    Key = hash(SSH, "E", 8*mac_key_bytes(SSH#ssh.recv_mac)),
		    {ok, SSH#ssh { recv_mac_key = Key }}
	    end;
	_ ->
	    %% Not applicable
	    {ok, SSH}
    end.

recv_mac_final(SSH) ->
    {ok, SSH#ssh { recv_mac = none, recv_mac_key = undefined }}.

mac(none, _ , _, _) ->
    <<>>;
mac('hmac-sha1', Key, SeqNum, Data) ->
    crypto:mac(hmac, sha, Key, [<<?UINT32(SeqNum)>>, Data]);
mac('hmac-sha1-96', Key, SeqNum, Data) ->
    crypto:macN(hmac, sha, Key, [<<?UINT32(SeqNum)>>, Data], mac_digest_size('hmac-sha1-96'));
mac('hmac-md5', Key, SeqNum, Data) ->
    crypto:mac(hmac, md5, Key, [<<?UINT32(SeqNum)>>, Data]);
mac('hmac-md5-96', Key, SeqNum, Data) ->
    crypto:macN(hmac, md5, Key, [<<?UINT32(SeqNum)>>, Data], mac_digest_size('hmac-md5-96'));
mac('hmac-sha2-256', Key, SeqNum, Data) ->
    crypto:mac(hmac, sha256, Key, [<<?UINT32(SeqNum)>>, Data]);
mac('hmac-sha2-512', Key, SeqNum, Data) ->
    crypto:mac(hmac, sha512, Key, [<<?UINT32(SeqNum)>>, Data]);
mac('hmac-sha1-etm@openssh.com', Key, SeqNum, Data) ->
    mac('hmac-sha1', Key, SeqNum, Data);
mac('hmac-sha2-256-etm@openssh.com', Key, SeqNum, Data) ->
    mac('hmac-sha2-256', Key, SeqNum, Data);
mac('hmac-sha2-512-etm@openssh.com', Key, SeqNum, Data) ->
    mac('hmac-sha2-512', Key, SeqNum, Data).


%%%----------------------------------------------------------------
%% return N hash bytes (HASH)
hash(_SSH, _Char, 0) ->
    <<>>;
hash(SSH, Char, N) ->
    HashAlg = sha(SSH#ssh.algorithms#alg.kex),
    K = SSH#ssh.shared_secret,
    H = SSH#ssh.exchanged_hash,
    K1 = crypto:hash(HashAlg, [K, H, Char,  SSH#ssh.session_id]),
    Sz = N div 8,
    <<Key:Sz/binary, _/binary>> = hash(K, H, K1, N-128, HashAlg),
    Key.

hash(_K, _H, Ki, N, _HashAlg) when N =< 0 ->
    Ki;
hash(K, H, Ki, N, HashAlg) ->
    Kj = crypto:hash(HashAlg, [K, H, Ki]),
    hash(K, H, <<Ki/binary, Kj/binary>>, N-128, HashAlg).

%%%----------------------------------------------------------------
kex_hash(SSH, Key, HashAlg, Args) ->
    crypto:hash(HashAlg, kex_plaintext(SSH,Key,Args)).


kex_plaintext(SSH, Key, Args) ->
    EncodedKey = ssh_message:ssh2_pubkey_encode(Key),
    <<?Estring(SSH#ssh.c_version), ?Estring(SSH#ssh.s_version),
      ?Ebinary(SSH#ssh.c_keyinit), ?Ebinary(SSH#ssh.s_keyinit),
      ?Ebinary(EncodedKey),
      (kex_alg_dependent(Args))/binary>>.


kex_alg_dependent({Q_c, Q_s, K}) when is_binary(Q_c), is_binary(Q_s) ->
    %% ecdh
    <<?Ebinary(Q_c), ?Ebinary(Q_s), ?Empint(K)>>;

kex_alg_dependent({E, F, K}) ->
    %% diffie-hellman
    <<?Empint(E), ?Empint(F), ?Empint(K)>>;

kex_alg_dependent({-1, NBits, -1, Prime, Gen, E, F, K}) ->
    %% ssh_msg_kex_dh_gex_request_old
    <<?Euint32(NBits),
      ?Empint(Prime), ?Empint(Gen), ?Empint(E), ?Empint(F), ?Empint(K)>>;

kex_alg_dependent({Min, NBits, Max, Prime, Gen, E, F, K}) ->
    %% diffie-hellman group exchange
    <<?Euint32(Min), ?Euint32(NBits), ?Euint32(Max),
      ?Empint(Prime), ?Empint(Gen), ?Empint(E), ?Empint(F), ?Empint(K)>>.

%%%----------------------------------------------------------------

valid_key_sha_alg(_, #{engine:=_, key_id:=_}, _Alg) -> true; % Engine key

valid_key_sha_alg(public, #'RSAPublicKey'{}, 'rsa-sha2-512') -> true;
valid_key_sha_alg(public, #'RSAPublicKey'{}, 'rsa-sha2-384') -> true;
valid_key_sha_alg(public, #'RSAPublicKey'{}, 'rsa-sha2-256') -> true;
valid_key_sha_alg(public, #'RSAPublicKey'{}, 'ssh-rsa'     ) -> true;

valid_key_sha_alg(private, #'RSAPrivateKey'{}, 'rsa-sha2-512') -> true;
valid_key_sha_alg(private, #'RSAPrivateKey'{}, 'rsa-sha2-384') -> true;
valid_key_sha_alg(private, #'RSAPrivateKey'{}, 'rsa-sha2-256') -> true;
valid_key_sha_alg(private, #'RSAPrivateKey'{}, 'ssh-rsa'     ) -> true;

valid_key_sha_alg(public, {_, #'Dss-Parms'{}}, 'ssh-dss') -> true;
valid_key_sha_alg(private, #'DSAPrivateKey'{},  'ssh-dss') -> true;

valid_key_sha_alg(public, {#'ECPoint'{},{namedCurve,OID}}, Alg) ->
    valid_key_sha_alg_ec(OID, Alg);
valid_key_sha_alg(private, #'ECPrivateKey'{parameters = {namedCurve,OID}}, Alg) ->
    valid_key_sha_alg_ec(OID, Alg);
valid_key_sha_alg(_, _, _) -> false.


valid_key_sha_alg_ec(OID, Alg) when is_tuple(OID) ->
    {SshCurveType, _} = ssh_message:oid2ssh_curvename(OID),
    Alg == binary_to_atom(SshCurveType);
valid_key_sha_alg_ec(_, _) -> false.

    

-dialyzer({no_match, public_algo/1}).

public_algo(#'RSAPublicKey'{}) ->   'ssh-rsa';  % FIXME: Not right with draft-curdle-rsa-sha2
public_algo({_, #'Dss-Parms'{}}) -> 'ssh-dss';
public_algo({#'ECPoint'{},{namedCurve,OID}}) when is_tuple(OID) -> 
    {SshCurveType, _} = ssh_message:oid2ssh_curvename(OID),
    binary_to_atom(SshCurveType).


sha('ssh-rsa') -> sha;
sha('rsa-sha2-256') -> sha256;
sha('rsa-sha2-384') -> sha384;
sha('rsa-sha2-512') -> sha512;
sha('ssh-dss') -> sha;
sha('ecdsa-sha2-nistp256') -> sha(secp256r1);
sha('ecdsa-sha2-nistp384') -> sha(secp384r1);
sha('ecdsa-sha2-nistp521') -> sha(secp521r1);
sha('ssh-ed25519') -> undefined; % Included in the spec of ed25519
sha('ssh-ed448') -> undefined; % Included in the spec of ed448
sha(secp256r1) -> sha256;
sha(secp384r1) -> sha384;
sha(secp521r1) -> sha512;
sha('diffie-hellman-group1-sha1') -> sha;
sha('diffie-hellman-group14-sha1') -> sha;
sha('diffie-hellman-group14-sha256') -> sha256;
sha('diffie-hellman-group16-sha512') -> sha512;
sha('diffie-hellman-group18-sha512') -> sha512;
sha('diffie-hellman-group-exchange-sha1')   -> sha;
sha('diffie-hellman-group-exchange-sha256') -> sha256;
sha(?'secp256r1') -> sha(secp256r1);
sha(?'secp384r1') -> sha(secp384r1);
sha(?'secp521r1') -> sha(secp521r1);
sha('ecdh-sha2-nistp256') -> sha(secp256r1);
sha('ecdh-sha2-nistp384') -> sha(secp384r1);
sha('ecdh-sha2-nistp521') -> sha(secp521r1);
sha('curve25519-sha256' ) -> sha256;
sha('curve25519-sha256@libssh.org' ) -> sha256;
sha('curve448-sha512') -> sha512;
sha(x25519) -> sha256;
sha(x448) -> sha512;
sha(Str) when is_list(Str), length(Str)<50 -> sha(list_to_existing_atom(Str)).


mac_key_bytes('hmac-sha1')    -> 20;
mac_key_bytes('hmac-sha1-etm@openssh.com') -> 20;
mac_key_bytes('hmac-sha1-96') -> 20;
mac_key_bytes('hmac-md5')     -> 16;
mac_key_bytes('hmac-md5-96')  -> 16;
mac_key_bytes('hmac-sha2-256')-> 32;
mac_key_bytes('hmac-sha2-256-etm@openssh.com')-> 32;
mac_key_bytes('hmac-sha2-512')-> 64;
mac_key_bytes('hmac-sha2-512-etm@openssh.com')-> 64;
mac_key_bytes('AEAD_AES_128_GCM') -> 0;
mac_key_bytes('AEAD_AES_256_GCM') -> 0;
mac_key_bytes('chacha20-poly1305@openssh.com') -> 0;
mac_key_bytes(none) -> 0.

mac_digest_size('hmac-sha1')    -> 20;
mac_digest_size('hmac-sha1-etm@openssh.com') -> 20;
mac_digest_size('hmac-sha1-96') -> 12;
mac_digest_size('hmac-md5')    -> 20;
mac_digest_size('hmac-md5-96') -> 12;
mac_digest_size('hmac-sha2-256') -> 32;
mac_digest_size('hmac-sha2-256-etm@openssh.com') -> 32;
mac_digest_size('hmac-sha2-512') -> 64;
mac_digest_size('hmac-sha2-512-etm@openssh.com') -> 64;
mac_digest_size('AEAD_AES_128_GCM') -> 16;
mac_digest_size('AEAD_AES_256_GCM') -> 16;
mac_digest_size('chacha20-poly1305@openssh.com') -> 16;
mac_digest_size(none) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Diffie-Hellman utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dh_group('diffie-hellman-group1-sha1') ->  ?dh_group1;
dh_group('diffie-hellman-group14-sha1') -> ?dh_group14;
dh_group('diffie-hellman-group14-sha256') -> ?dh_group14;
dh_group('diffie-hellman-group16-sha512') -> ?dh_group16;
dh_group('diffie-hellman-group18-sha512') -> ?dh_group18.

%%%----------------------------------------------------------------
parallell_gen_key(Ssh = #ssh{keyex_key = {x, {G, P}},
                             algorithms = Algs}) ->
    Sz = dh_bits(Algs),
    {Public, Private} = generate_key(dh, [P,G,2*Sz]),
    Ssh#ssh{keyex_key = {{Private, Public}, {G, P}}}.


generate_key(ecdh, Args) ->
    crypto:generate_key(ecdh, Args);
generate_key(dh, [P,G,Sz2]) ->
    {Public,Private} = crypto:generate_key(dh, [P, G, max(Sz2,?MIN_DH_KEY_SIZE)] ),
    {crypto:bytes_to_integer(Public), crypto:bytes_to_integer(Private)}.


compute_key(Algorithm, OthersPublic, MyPrivate, Args) ->
    Shared = crypto:compute_key(Algorithm, OthersPublic, MyPrivate, Args),
    crypto:bytes_to_integer(Shared).


dh_bits(#alg{encrypt = Encrypt,
             send_mac = SendMac}) ->
    C = cipher(Encrypt),
    8 * lists:max([C#cipher.key_bytes,
                   C#cipher.block_bytes,
                   C#cipher.iv_bytes,
                   mac_key_bytes(SendMac)
                  ]).

ecdh_curve('ecdh-sha2-nistp256') -> secp256r1;
ecdh_curve('ecdh-sha2-nistp384') -> secp384r1;
ecdh_curve('ecdh-sha2-nistp521') -> secp521r1;
ecdh_curve('curve448-sha512'   ) -> x448;
ecdh_curve('curve25519-sha256' ) -> x25519;
ecdh_curve('curve25519-sha256@libssh.org' ) -> x25519.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utils for default_algorithms/1 and supported_algorithms/1
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

supported_algorithms(Key, [{client2server,BL1},{server2client,BL2}]) ->
    [{client2server,As1},{server2client,As2}] = supported_algorithms(Key),
    [{client2server,As1--BL1},{server2client,As2--BL2}];
supported_algorithms(Key, BlackList) ->
    supported_algorithms(Key) -- BlackList.


select_crypto_supported(L) ->    
    Sup = crypto:supports(),
    [Name || {Name,CryptoRequires} <- L,
	     crypto_supported(CryptoRequires, Sup)].

crypto_supported(Conditions, Supported) ->
    lists:all( fun({Tag,CryptoName}) when is_atom(CryptoName) ->
		       crypto_name_supported(Tag,CryptoName,Supported)
	       end, Conditions).

crypto_name_supported(Tag, CryptoName, Supported) ->
    Vs = proplists:get_value(Tag,Supported,[]),
    lists:member(CryptoName, Vs).

same(Algs) ->  [{client2server,Algs}, {server2client,Algs}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Other utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-------- Remove CR, LF and following characters from a line

trim_tail(Str) ->
    lists:takewhile(fun(C) -> 
			    C=/=$\r andalso C=/=$\n
		    end, Str).

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [alg, ssh_messages, raw_messages, hello].

ssh_dbg_flags(alg) -> [c];
ssh_dbg_flags(hello) -> [c];
ssh_dbg_flags(raw_messages) -> ssh_dbg_flags(hello);
ssh_dbg_flags(ssh_messages) -> ssh_dbg_flags(hello).


ssh_dbg_on(alg) -> dbg:tpl(?MODULE,select_algorithm,4,x);
ssh_dbg_on(hello) -> dbg:tp(?MODULE,hello_version_msg,1,x),
                     dbg:tp(?MODULE,handle_hello_version,1,x);
ssh_dbg_on(raw_messages) -> ssh_dbg_on(hello);
ssh_dbg_on(ssh_messages) -> ssh_dbg_on(hello).


ssh_dbg_off(alg) -> dbg:ctpl(?MODULE,select_algorithm,4);
ssh_dbg_off(hello) -> dbg:ctpg(?MODULE,hello_version_msg,1),
                      dbg:ctpg(?MODULE,handle_hello_version,1);
ssh_dbg_off(raw_messages) -> ssh_dbg_off(hello);
ssh_dbg_off(ssh_messages) -> ssh_dbg_off(hello).




ssh_dbg_format(hello, {call,{?MODULE,hello_version_msg,[_]}}) ->
    skip;
ssh_dbg_format(hello, {return_from,{?MODULE,hello_version_msg,1},Hello}) ->
    ["Going to send hello message:\n",
     Hello
    ];

ssh_dbg_format(hello, {call,{?MODULE,handle_hello_version,[Hello]}}) ->
    ["Received hello message:\n",
     Hello
    ];
ssh_dbg_format(hello, {return_from,{?MODULE,handle_hello_version,1},_Ret}) ->
    skip;

ssh_dbg_format(alg, {call,{?MODULE,select_algorithm,[_,_,_,_]}}) ->
    skip;
ssh_dbg_format(alg, {return_from,{?MODULE,select_algorithm,4},{ok,Alg}}) ->
    ["Negotiated algorithms:\n",
     wr_record(Alg)
    ];

ssh_dbg_format(raw_messages, X) -> ssh_dbg_format(hello, X);
ssh_dbg_format(ssh_messages, X) -> ssh_dbg_format(hello, X).

?wr_record(alg).
