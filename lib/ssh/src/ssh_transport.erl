%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
	 handle_packet_part/4,
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
	 extract_public_key/1,
	 ssh_packet/2, pack/2,
         valid_key_sha_alg/2,
	 sha/1, sign/3, verify/4]).

%%% For test suites
-export([pack/3]).
-export([decompress/2,  decrypt_blocks/3, is_valid_mac/3 ]). % FIXME: remove

-define(Estring(X), ?STRING((if is_binary(X) -> X;
				is_list(X) -> list_to_binary(X);
				X==undefined -> <<>>
			     end))).
-define(Empint(X),     (ssh_bits:mpint(X))/binary ).
-define(Ebinary(X),    ?STRING(X) ).
-define(Euint32(X),   ?UINT32(X) ).

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

default_algorithms() -> [{K,default_algorithms(K)} || K <- algo_classes()].

algo_classes() -> [kex, public_key, cipher, mac, compression].

default_algorithms(kex) ->
    supported_algorithms(kex, [
                               'diffie-hellman-group1-sha1' % Gone in OpenSSH 7.3.p1
                              ]);

default_algorithms(cipher) ->
    supported_algorithms(cipher, same(['AEAD_AES_128_GCM',
				       'AEAD_AES_256_GCM']));
default_algorithms(mac) ->
    supported_algorithms(mac, same(['AEAD_AES_128_GCM',
				    'AEAD_AES_256_GCM']));
default_algorithms(public_key) ->
    supported_algorithms(public_key, ['rsa-sha2-256',
                                      'rsa-sha2-384',
                                      'rsa-sha2-512']);
default_algorithms(Alg) ->
    supported_algorithms(Alg, []).


supported_algorithms() -> [{K,supported_algorithms(K)} || K <- algo_classes()].

supported_algorithms(kex) ->
    select_crypto_supported(
      [
       {'ecdh-sha2-nistp384',                   [{public_keys,ecdh}, {ec_curve,secp384r1}, {hashs,sha384}]},
       {'ecdh-sha2-nistp521',                   [{public_keys,ecdh}, {ec_curve,secp521r1}, {hashs,sha512}]},
       {'ecdh-sha2-nistp256',                   [{public_keys,ecdh}, {ec_curve,secp256r1}, {hashs,sha256}]},
       {'diffie-hellman-group-exchange-sha256', [{public_keys,dh},   {hashs,sha256}]},
       {'diffie-hellman-group16-sha512',        [{public_keys,dh},   {hashs,sha512}]}, % In OpenSSH 7.3.p1
       {'diffie-hellman-group18-sha512',        [{public_keys,dh},   {hashs,sha512}]}, % In OpenSSH 7.3.p1
       {'diffie-hellman-group14-sha256',        [{public_keys,dh},   {hashs,sha256}]}, % In OpenSSH 7.3.p1
       {'diffie-hellman-group14-sha1',          [{public_keys,dh},   {hashs,sha}]},
       {'diffie-hellman-group-exchange-sha1',   [{public_keys,dh},   {hashs,sha}]},
       {'diffie-hellman-group1-sha1',           [{public_keys,dh},   {hashs,sha}]}
      ]);
supported_algorithms(public_key) ->
    select_crypto_supported(
      [
       {'ecdsa-sha2-nistp384',  [{public_keys,ecdsa}, {hashs,sha384}, {ec_curve,secp384r1}]},
       {'ecdsa-sha2-nistp521',  [{public_keys,ecdsa}, {hashs,sha512}, {ec_curve,secp521r1}]},
       {'ecdsa-sha2-nistp256',  [{public_keys,ecdsa}, {hashs,sha256}, {ec_curve,secp256r1}]},
       {'rsa-sha2-256',         [{public_keys,rsa},   {hashs,sha256}                      ]},
       {'rsa-sha2-384',         [{public_keys,rsa},   {hashs,sha384}                      ]},
       {'rsa-sha2-512',         [{public_keys,rsa},   {hashs,sha512}                      ]},
       {'ssh-rsa',              [{public_keys,rsa},   {hashs,sha}                         ]},
       {'ssh-dss',              [{public_keys,dss},   {hashs,sha}                         ]} % Gone in OpenSSH 7.3.p1
      ]);
 
supported_algorithms(cipher) ->
    same(
      select_crypto_supported(
	[
         {'aes256-gcm@openssh.com', [{ciphers,{aes_gcm,256}}]},
         {'aes256-ctr',       [{ciphers,{aes_ctr,256}}]},
         {'aes192-ctr',       [{ciphers,{aes_ctr,192}}]},
	 {'aes128-gcm@openssh.com', [{ciphers,{aes_gcm,128}}]},
	 {'aes128-ctr',       [{ciphers,{aes_ctr,128}}]},
	 {'AEAD_AES_256_GCM', [{ciphers,{aes_gcm,256}}]},
	 {'AEAD_AES_128_GCM', [{ciphers,{aes_gcm,128}}]},
	 {'aes128-cbc',       [{ciphers,aes_cbc128}]},
	 {'3des-cbc',         [{ciphers,des3_cbc}]}
	]
       ));
supported_algorithms(mac) ->
    same(
      select_crypto_supported(
	[{'hmac-sha2-256',    [{hashs,sha256}]},
	 {'hmac-sha2-512',    [{hashs,sha512}]},
	 {'hmac-sha1',        [{hashs,sha}]},
	 {'AEAD_AES_128_GCM', [{ciphers,{aes_gcm,128}}]},
	 {'AEAD_AES_256_GCM', [{ciphers,{aes_gcm,256}}]}
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

software_version(Options) -> 
    case ?GET_OPT(id_string, Options) of
	undefined ->
	    "Erlang"++ssh_vsn();
	{random,Nlo,Nup} ->
	    random_id(Nlo,Nup);
	ID ->
	    ID
    end.

ssh_vsn() ->
    try {ok,L} = application:get_all_key(ssh),
	 proplists:get_value(vsn, L, "")
    of 
	"" -> "";
	VSN when is_list(VSN) -> "/" ++ VSN;
	_ -> ""
    catch
	_:_ -> ""
    end.
    
random_id(Nlo, Nup) ->
    [$a + rand:uniform($z-$a+1) - 1 || _<- lists:duplicate(Nlo + rand:uniform(Nup-Nlo+1) - 1, x)].

hello_version_msg(Data) ->
    [Data,"\r\n"].

next_seqnum(SeqNum) ->
    (SeqNum + 1) band 16#ffffffff.

decrypt_blocks(Bin, Length, Ssh0) ->
    <<EncBlocks:Length/binary, EncData/binary>> = Bin,
    {Ssh, DecData} = decrypt(Ssh0, EncBlocks),
    {Ssh, DecData, EncData}.

is_valid_mac(_, _ , #ssh{recv_mac_size = 0}) ->
    true;
is_valid_mac(Mac, Data, #ssh{recv_mac = Algorithm,
			     recv_mac_key = Key, recv_sequence = SeqNum}) ->
    Mac == mac(Algorithm, Key, SeqNum, Data).

format_version({Major,Minor}, SoftwareVersion) ->
    "SSH-" ++ integer_to_list(Major) ++ "." ++ 
	integer_to_list(Minor) ++ "-" ++ SoftwareVersion.

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

kex_init(#ssh{role = Role, opts = Opts, available_host_keys = HostKeyAlgs}) ->
    Random = ssh_bits:random(16),
    PrefAlgs = ?GET_OPT(preferred_algorithms, Opts),
    kexinit_message(Role, Random, PrefAlgs, HostKeyAlgs, Opts).

key_init(client, Ssh, Value) ->
    Ssh#ssh{c_keyinit = Value};
key_init(server, Ssh, Value) ->
    Ssh#ssh{s_keyinit = Value}.


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

get_algs(Key, Algs) -> proplists:get_value(Key, Algs, default_algorithms(Key)).

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
        _:_ ->
            ssh_connection_handler:disconnect(
              #ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                  description = "Selection of key exchange algorithm failed"})
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
        _:_ ->
            ssh_connection_handler:disconnect(
              #ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                  description = "Selection of key exchange algorithm failed"})
    end.


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
				       Kex == 'ecdh-sha2-nistp521' ->
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
                                                hkey=SignAlg} = Algs}) ->
    %% server
    {G, P} = dh_group(Kex),
    if
	1=<E, E=<(P-1) ->
            Sz = dh_bits(Algs),
	    {Public, Private} = generate_key(dh, [P,G,2*Sz]),
	    K = compute_key(dh, E, Private, [P,G]),
	    MyPrivHostKey = get_host_key(Ssh0, SignAlg),
	    MyPubHostKey = extract_public_key(MyPrivHostKey),
            H = kex_hash(Ssh0, MyPubHostKey, SignAlg, sha(Kex), {E,Public,K}),
            H_SIG = sign(H, sha(SignAlg), MyPrivHostKey),
	    {SshPacket, Ssh1} = 
		ssh_packet(#ssh_msg_kexdh_reply{public_host_key = {MyPubHostKey,SignAlg},
						f = Public,
						h_sig = H_SIG
					       }, Ssh0),
	    {ok, SshPacket, Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}},
				     shared_secret = ssh_bits:mpint(K),
				     exchanged_hash = H,
				     session_id = sid(Ssh1, H)}};

	true ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Key exchange failed, 'e' out of bounds"},
	      {error,bad_e_from_peer}
	     )
    end.

handle_kexdh_reply(#ssh_msg_kexdh_reply{public_host_key = PeerPubHostKey,
					f = F,
					h_sig = H_SIG}, 
		   #ssh{keyex_key = {{Private, Public}, {G, P}},
                        algorithms = #alg{kex=Kex,
                                          hkey=SignAlg}} = Ssh0) ->
    %% client
    if 
	1=<F, F=<(P-1)->
	    K = compute_key(dh, F, Private, [P,G]),
            H = kex_hash(Ssh0, PeerPubHostKey, SignAlg, sha(Kex), {Public,F,K}),
	    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
		ok ->
		    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
		    {ok, SshPacket, install_alg(snd, Ssh#ssh{shared_secret  = ssh_bits:mpint(K),
                                                             exchanged_hash = H,
                                                             session_id = sid(Ssh, H)})};
		Error ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Key exchange failed"},
	      Error)
	    end;

	true ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Key exchange failed, 'f' out of bounds"},
	      bad_f_from_peer
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
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
		 description = "No possible diffie-hellman-group-exchange group found"
		})
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
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
		 description = "No possible diffie-hellman-group-exchange group found"
		})
    end;

handle_kex_dh_gex_request(_, _) ->
    ssh_connection_handler:disconnect(
	 #ssh_msg_disconnect{
	    code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
	    description = "Key exchange failed, bad values in ssh_msg_kex_dh_gex_request"},
      bad_ssh_msg_kex_dh_gex_request).


adjust_gex_min_max(Min0, Max0, Opts) ->
    {Min1, Max1} = ?GET_OPT(dh_gex_limits, Opts),
    Min2 = max(Min0, Min1),
    Max2 = min(Max0, Max1),
    if
        Min2 =< Max2 ->
            {Min2, Max2};
        Max2 < Min2 ->
            ssh_connection_handler:disconnect(
              #ssh_msg_disconnect{
                 code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
                 description = "No possible diffie-hellman-group-exchange group possible"
                })
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
                                              hkey=SignAlg}} = Ssh0) ->
    %% server
    if
	1=<E, E=<(P-1) ->
	    K = compute_key(dh, E, Private, [P,G]),
	    if
		1<K, K<(P-1) ->
		    MyPrivHostKey = get_host_key(Ssh0, SignAlg),
		    MyPubHostKey = extract_public_key(MyPrivHostKey),
                    H = kex_hash(Ssh0, MyPubHostKey, SignAlg, sha(Kex), {Min,NBits,Max,P,G,E,Public,K}),
                    H_SIG = sign(H, sha(SignAlg), MyPrivHostKey),
		    {SshPacket, Ssh} = 
			ssh_packet(#ssh_msg_kex_dh_gex_reply{public_host_key = {MyPubHostKey,SignAlg},
							     f = Public,
							     h_sig = H_SIG}, Ssh0),
		    {ok, SshPacket, Ssh#ssh{shared_secret = ssh_bits:mpint(K),
					    exchanged_hash = H,
					    session_id = sid(Ssh, H)
					   }};
		true ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Key exchange failed, 'K' out of bounds"},
	      bad_K)
	    end;
	true ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Key exchange failed, 'e' out of bounds"},
	      bad_e_from_peer)
    end.

handle_kex_dh_gex_reply(#ssh_msg_kex_dh_gex_reply{public_host_key = PeerPubHostKey, 
						  f = F,
						  h_sig = H_SIG},
			#ssh{keyex_key = {{Private, Public}, {G, P}},
			     keyex_info = {Min, Max, NBits},
                             algorithms = #alg{kex=Kex,
                                               hkey=SignAlg}} = 
			    Ssh0) ->
    %% client
    if 
	1=<F, F=<(P-1)->
	    K = compute_key(dh, F, Private, [P,G]),
	    if
		1<K, K<(P-1) ->
                    H = kex_hash(Ssh0, PeerPubHostKey, SignAlg, sha(Kex), {Min,NBits,Max,P,G,Public,F,K}),
		    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
			ok ->
			    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
			    {ok, SshPacket, install_alg(snd, Ssh#ssh{shared_secret  = ssh_bits:mpint(K),
                                                                     exchanged_hash = H,
                                                                     session_id = sid(Ssh, H)})};
			_Error ->
			    ssh_connection_handler:disconnect(
			      #ssh_msg_disconnect{
				 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
				 description = "Key exchange failed"
				})
		    end;

		true ->
		    ssh_connection_handler:disconnect(
		      #ssh_msg_disconnect{
			 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			 description = "Key exchange failed, 'K' out of bounds"},
		      bad_K)
	    end;
	true ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Key exchange failed, 'f' out of bounds"},
	      bad_f_from_peer
	     )
    end.

%%%----------------------------------------------------------------
%%%
%%% diffie-hellman-ecdh-sha2-*
%%% 
handle_kex_ecdh_init(#ssh_msg_kex_ecdh_init{q_c = PeerPublic},
		     Ssh0 = #ssh{algorithms = #alg{kex=Kex,
                                                   hkey=SignAlg}}) ->
    %% at server
    Curve = ecdh_curve(Kex),
    {MyPublic, MyPrivate} = generate_key(ecdh, Curve),
    try
	compute_key(ecdh, PeerPublic, MyPrivate, Curve)
    of
	K ->
	    MyPrivHostKey = get_host_key(Ssh0, SignAlg),
	    MyPubHostKey = extract_public_key(MyPrivHostKey),
            H = kex_hash(Ssh0, MyPubHostKey, SignAlg, sha(Curve), {PeerPublic, MyPublic, K}),
            H_SIG = sign(H, sha(SignAlg), MyPrivHostKey),
	    {SshPacket, Ssh1} = 
		ssh_packet(#ssh_msg_kex_ecdh_reply{public_host_key = {MyPubHostKey,SignAlg},
						   q_s = MyPublic,
						   h_sig = H_SIG},
			   Ssh0),
    	    {ok, SshPacket, Ssh1#ssh{keyex_key = {{MyPublic,MyPrivate},Curve},
				     shared_secret = ssh_bits:mpint(K),
				     exchanged_hash = H,
				     session_id = sid(Ssh1, H)}}
    catch
	_:_ ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Peer ECDH public key is invalid"},
	      invalid_peer_public_key)
    end.

handle_kex_ecdh_reply(#ssh_msg_kex_ecdh_reply{public_host_key = PeerPubHostKey,
					      q_s = PeerPublic,
					      h_sig = H_SIG},
		      #ssh{keyex_key = {{MyPublic,MyPrivate}, Curve},
                           algorithms = #alg{hkey=SignAlg}} = Ssh0
		     ) ->
    %% at client
    try
	compute_key(ecdh, PeerPublic, MyPrivate, Curve)
    of
	K ->
            H = kex_hash(Ssh0, PeerPubHostKey,  SignAlg, sha(Curve), {MyPublic,PeerPublic,K}),
	    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
		ok ->
		    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
		    {ok, SshPacket, install_alg(snd, Ssh#ssh{shared_secret  = ssh_bits:mpint(K),
                                                             exchanged_hash = H,
                                                             session_id = sid(Ssh, H)})};
		Error ->
		    ssh_connection_handler:disconnect(
		       #ssh_msg_disconnect{
			  code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			  description = "Key exchange failed"},
		       Error)
	    end
    catch
	_:_ ->
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{
		 code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		 description = "Peer ECDH public key is invalid"},
	      invalid_peer_public_key)
    end.


%%%----------------------------------------------------------------
handle_new_keys(#ssh_msg_newkeys{}, Ssh0) ->
    try install_alg(rcv, Ssh0) of
	#ssh{} = Ssh ->
	    {ok, Ssh}
    catch 
	_C:_Error -> %% TODO: Throw earlier ....
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				  description = "Install alg failed"
				 })
    end. 


%%%----------------------------------------------------------------
kex_ext_info(Role, Opts) ->
    case ?GET_OPT(recv_ext_info,Opts) of
        true when Role==client -> ["ext-info-c"];
        true when Role==server -> ["ext-info-s"];
        false -> []
    end.
    
ext_info_message(#ssh{role=client,
                      send_ext_info=true} = Ssh0) ->
    %% FIXME: no extensions implemented
    {ok, "", Ssh0};

ext_info_message(#ssh{role=server,
                      send_ext_info=true} = Ssh0) ->
    AlgsList = lists:map(fun erlang:atom_to_list/1,
                         ssh_transport:default_algorithms(public_key)),
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
get_host_key(SSH, SignAlg) ->
    #ssh{key_cb = {KeyCb,KeyCbOpts}, opts = Opts} = SSH,
    UserOpts = ?GET_OPT(user_options, Opts),
    case KeyCb:host_key(SignAlg, [{key_cb_private,KeyCbOpts}|UserOpts]) of
	{ok, PrivHostKey} ->  PrivHostKey;
	Result -> exit({error, {Result, unsupported_key_type}})
    end.

extract_public_key(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #'RSAPublicKey'{modulus = N, publicExponent = E};
extract_public_key(#'DSAPrivateKey'{y = Y, p = P, q = Q, g = G}) ->
    {Y,  #'Dss-Parms'{p=P, q=Q, g=G}};
extract_public_key(#'ECPrivateKey'{parameters = {namedCurve,OID},
				   publicKey = Q}) ->
    {#'ECPoint'{point=Q}, {namedCurve,OID}}.


verify_host_key(#ssh{algorithms=Alg}=SSH, PublicKey, Digest, Signature) ->
    case verify(Digest, sha(Alg#alg.hkey), Signature, PublicKey) of
	false ->
	    {error, bad_signature};
	true ->
	    known_host_key(SSH, PublicKey, public_algo(PublicKey))
    end.



accepted_host(Ssh, PeerName, Public, Opts) ->
    case ?GET_OPT(silently_accept_hosts, Opts) of

        %% Original option values; User question and no host key fingerprints known.
        %% Keep the original question unchanged:
	false -> yes == yes_no(Ssh, "New host " ++ PeerName ++ " accept");
	true -> true;

        %% Variant: User question but with host key fingerprint in the question:
        {false,Alg} ->
            HostKeyAlg = (Ssh#ssh.algorithms)#alg.hkey,
            Prompt = io_lib:format("The authenticity of the host can't be established.~n"
                                   "~s host key fingerprint is ~s.~n"
                                   "New host ~p accept",
                                   [fmt_hostkey(HostKeyAlg),
                                    public_key:ssh_hostkey_fingerprint(Alg,Public),
                                    PeerName]),
            yes == yes_no(Ssh, Prompt);

        %% Call-back alternatives: A user provided fun is called for the decision:
        F when is_function(F,2) ->
	    true == (catch F(PeerName, public_key:ssh_hostkey_fingerprint(Public)));

	{DigestAlg,F} when is_function(F,2) ->
	    true == (catch F(PeerName, public_key:ssh_hostkey_fingerprint(DigestAlg,Public)))
        
    end.


yes_no(Ssh, Prompt)  ->
    (Ssh#ssh.io_cb):yes_no(Prompt, Ssh#ssh.opts).


fmt_hostkey('ssh-rsa') -> "RSA";
fmt_hostkey('ssh-dss') -> "DSA";
fmt_hostkey(A) when is_atom(A) -> fmt_hostkey(atom_to_list(A));
fmt_hostkey("ecdsa"++_) -> "ECDSA";
fmt_hostkey(X) -> X.


known_host_key(#ssh{opts = Opts, key_cb = {KeyCb,KeyCbOpts}, peer = {PeerName,_}} = Ssh, 
	       Public, Alg) ->
    UserOpts = ?GET_OPT(user_options, Opts),
    case KeyCb:is_host_key(Public, PeerName, Alg, [{key_cb_private,KeyCbOpts}|UserOpts]) of
	true ->
	    ok;
	false ->
	    case accepted_host(Ssh, PeerName, Public, Opts) of
		true ->
		    KeyCb:add_host_key(PeerName, Public, [{key_cb_private,KeyCbOpts}|UserOpts]);
		false ->
		    {error, rejected}
	    end
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

aead_gcm_simultan('aes128-gcm@openssh.com', _) -> {'AEAD_AES_128_GCM', 'AEAD_AES_128_GCM'};
aead_gcm_simultan('aes256-gcm@openssh.com', _) -> {'AEAD_AES_256_GCM', 'AEAD_AES_256_GCM'};
aead_gcm_simultan('AEAD_AES_128_GCM', _) -> {'AEAD_AES_128_GCM', 'AEAD_AES_128_GCM'};
aead_gcm_simultan('AEAD_AES_256_GCM', _) -> {'AEAD_AES_256_GCM', 'AEAD_AES_256_GCM'};
aead_gcm_simultan(_, 'AEAD_AES_128_GCM') -> {'AEAD_AES_128_GCM', 'AEAD_AES_128_GCM'};
aead_gcm_simultan(_, 'AEAD_AES_256_GCM') -> {'AEAD_AES_256_GCM', 'AEAD_AES_256_GCM'};
aead_gcm_simultan(Cipher, Mac) -> {Cipher,Mac}.


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
    SSH#ssh{kex = ALG#alg.kex,
	    hkey = ALG#alg.hkey,
	    encrypt = ALG#alg.encrypt,
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
    SSH#ssh{kex = ALG#alg.kex,
	    hkey = ALG#alg.hkey,
	    decrypt = ALG#alg.decrypt,
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
    A = CL -- SL,  %% algortihms only used by client
    %% algorithms used by client and server (client pref)
    lists:map(fun(ALG) -> list_to_atom(ALG) end, (CL -- A));
select_all(CL, SL) ->
    Err = lists:concat(["Received too many algorithms (",length(CL),"+",length(SL)," >= ",?MAX_NUM_ALGORITHMS,")."]),
    ssh_connection_handler:disconnect(
      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
			  description = Err}).


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
	  send_mac_key = MacKey,
	  encrypt = CryptoAlg} = Ssh0,  PacketLenDeviationForTests) when is_binary(PlainText) ->

    {Ssh1, CompressedPlainText} = compress(Ssh0, PlainText),
    {EcryptedPacket, MAC, Ssh3} =
	case pkt_type(CryptoAlg) of
	    common ->
		PaddingLen = padding_length(4+1+size(CompressedPlainText), Ssh0),
		Padding =  ssh_bits:random(PaddingLen),
		PlainPacketLen = 1 + PaddingLen + size(CompressedPlainText) + PacketLenDeviationForTests,
		PlainPacketData = <<?UINT32(PlainPacketLen),?BYTE(PaddingLen), CompressedPlainText/binary, Padding/binary>>,
		{Ssh2, EcryptedPacket0} = encrypt(Ssh1, PlainPacketData),
		MAC0 = mac(MacAlg, MacKey, SeqNum, PlainPacketData),
		{EcryptedPacket0, MAC0, Ssh2};
	    aead ->
		PaddingLen = padding_length(1+size(CompressedPlainText), Ssh0),
		Padding =  ssh_bits:random(PaddingLen),
		PlainPacketLen = 1 + PaddingLen + size(CompressedPlainText) + PacketLenDeviationForTests,
		PlainPacketData = <<?BYTE(PaddingLen), CompressedPlainText/binary, Padding/binary>>,
		{Ssh2, {EcryptedPacket0,MAC0}} = encrypt(Ssh1, {<<?UINT32(PlainPacketLen)>>,PlainPacketData}),
		{<<?UINT32(PlainPacketLen),EcryptedPacket0/binary>>, MAC0, Ssh2}
	end,
    FinalPacket = [EcryptedPacket, MAC],
    Ssh = Ssh3#ssh{send_sequence = (SeqNum+1) band 16#ffffffff},
    {FinalPacket, Ssh}.


padding_length(Size, #ssh{encrypt_block_size = BlockSize,
			  random_length_padding = RandomLengthPadding}) ->
    PL = (BlockSize - (Size rem BlockSize)) rem BlockSize,
    MinPaddingLen = if PL <  4 -> PL + BlockSize;
		       true -> PL
		    end,
    PadBlockSize =  max(BlockSize,4),
    MaxExtraBlocks = (max(RandomLengthPadding,MinPaddingLen) - MinPaddingLen) div PadBlockSize,
    ExtraPaddingLen = try (rand:uniform(MaxExtraBlocks+1) - 1) * PadBlockSize
		      catch _:_ -> 0
		      end,
    MinPaddingLen + ExtraPaddingLen.



handle_packet_part(<<>>, Encrypted0, undefined, #ssh{decrypt = CryptoAlg} = Ssh0) ->
    %% New ssh packet
    case get_length(pkt_type(CryptoAlg), Encrypted0, Ssh0) of
	get_more ->
	    %% too short to get the length
	    {get_more, <<>>, Encrypted0, undefined, Ssh0};

	{ok, PacketLen, _, _, _} when PacketLen > ?SSH_MAX_PACKET_SIZE ->
	    %% far too long message than expected
	    {error, {exceeds_max_size,PacketLen}};
	
	{ok, PacketLen, Decrypted, Encrypted1,
	 #ssh{recv_mac_size = MacSize} = Ssh1} ->
	    %% enough bytes so we got the length and can calculate how many
	    %% more bytes to expect for a full packet
	    TotalNeeded = (4 + PacketLen + MacSize),
	    handle_packet_part(Decrypted, Encrypted1, TotalNeeded, Ssh1)
    end;

handle_packet_part(DecryptedPfx, EncryptedBuffer, TotalNeeded, Ssh0) 
  when (size(DecryptedPfx)+size(EncryptedBuffer)) < TotalNeeded ->
    %% need more bytes to finalize the packet
    {get_more, DecryptedPfx, EncryptedBuffer, TotalNeeded, Ssh0};

handle_packet_part(DecryptedPfx, EncryptedBuffer, TotalNeeded, 
		   #ssh{recv_mac_size = MacSize,
			decrypt = CryptoAlg} = Ssh0) ->
    %% enough bytes to decode the packet.
    DecryptLen = TotalNeeded - size(DecryptedPfx) - MacSize,
    <<EncryptedSfx:DecryptLen/binary, Mac:MacSize/binary, NextPacketBytes/binary>> = EncryptedBuffer,
    case pkt_type(CryptoAlg) of
	common ->
	    {Ssh1, DecryptedSfx} = decrypt(Ssh0, EncryptedSfx),
	    DecryptedPacket = <<DecryptedPfx/binary, DecryptedSfx/binary>>,
	    case is_valid_mac(Mac, DecryptedPacket, Ssh1) of
		false ->
		    {bad_mac, Ssh1};
		true ->
		    {Ssh, DecompressedPayload} = decompress(Ssh1, payload(DecryptedPacket)),
		    {packet_decrypted, DecompressedPayload, NextPacketBytes, Ssh}
	    end;
	aead ->
	    PacketLenBin = DecryptedPfx,
	    case decrypt(Ssh0, {PacketLenBin,EncryptedSfx,Mac}) of
		{Ssh1, error} ->
		    {bad_mac, Ssh1};
		{Ssh1, DecryptedSfx} ->
                    DecryptedPacket = <<DecryptedPfx/binary, DecryptedSfx/binary>>,
		    {Ssh, DecompressedPayload} = decompress(Ssh1, payload(DecryptedPacket)),
		    {packet_decrypted, DecompressedPayload, NextPacketBytes, Ssh}
	    end
    end.
    
    
get_length(common, EncryptedBuffer, #ssh{decrypt_block_size = BlockSize} = Ssh0) ->
    case size(EncryptedBuffer) >= erlang:max(8, BlockSize) of
	true ->
	    <<EncBlock:BlockSize/binary, EncryptedRest/binary>> = EncryptedBuffer,
	    {Ssh, 
	     <<?UINT32(PacketLen),_/binary>> = Decrypted} = decrypt(Ssh0, EncBlock),
	    {ok, PacketLen, Decrypted, EncryptedRest, Ssh};
	false ->
	    get_more
    end;
get_length(aead, EncryptedBuffer, Ssh) ->
    case size(EncryptedBuffer) >= 4 of
	true ->
	    <<?UINT32(PacketLen), EncryptedRest/binary>> = EncryptedBuffer,
	    {ok, PacketLen, <<?UINT32(PacketLen)>>, EncryptedRest, Ssh};
	false ->
	    get_more
    end.

pkt_type('AEAD_AES_128_GCM') -> aead;
pkt_type('AEAD_AES_256_GCM') -> aead;
pkt_type(_) -> common.

payload(<<PacketLen:32, PaddingLen:8, PayloadAndPadding/binary>>) ->
    PayloadLen = PacketLen - PaddingLen - 1,
    <<Payload:PayloadLen/binary, _/binary>> = PayloadAndPadding,
    Payload.

sign(SigData, HashAlg,  #'DSAPrivateKey'{} = Key) ->
    DerSignature = public_key:sign(SigData, HashAlg, Key),
    #'Dss-Sig-Value'{r = R, s = S} = public_key:der_decode('Dss-Sig-Value', DerSignature),
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>>;
sign(SigData, HashAlg, Key = #'ECPrivateKey'{}) ->
    DerEncodedSign =  public_key:sign(SigData, HashAlg, Key),
    #'ECDSA-Sig-Value'{r=R, s=S} = public_key:der_decode('ECDSA-Sig-Value', DerEncodedSign),
    <<?Empint(R),?Empint(S)>>;
sign(SigData, HashAlg, Key) ->
    public_key:sign(SigData, HashAlg, Key).

verify(PlainText, HashAlg, Sig, {_,  #'Dss-Parms'{}} = Key) ->
    case Sig of
        <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>> ->
            Signature = public_key:der_encode('Dss-Sig-Value', #'Dss-Sig-Value'{r = R, s = S}),
            public_key:verify(PlainText, HashAlg, Signature, Key);
        _ ->
            false
    end;
verify(PlainText, HashAlg, Sig, {#'ECPoint'{},_} = Key) ->
    case Sig of
        <<?UINT32(Rlen),R:Rlen/big-signed-integer-unit:8,
          ?UINT32(Slen),S:Slen/big-signed-integer-unit:8>> ->
            Sval = #'ECDSA-Sig-Value'{r=R, s=S},
            DerEncodedSig = public_key:der_encode('ECDSA-Sig-Value',Sval),
            public_key:verify(PlainText, HashAlg, DerEncodedSig, Key);
        _ ->
            false
    end;
verify(PlainText, HashAlg, Sig, Key) ->
    public_key:verify(PlainText, HashAlg, Sig, Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Encryption
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Unit: bytes

-record(cipher_data, {
          key_bytes,
          iv_bytes,
          block_bytes
         }).

%%% Start of a more parameterized crypto handling.
cipher('AEAD_AES_128_GCM') ->
    #cipher_data{key_bytes = 16,
                 iv_bytes = 12,
                 block_bytes = 16};

cipher('AEAD_AES_256_GCM') ->
    #cipher_data{key_bytes = 32,
                 iv_bytes = 12,
                 block_bytes = 16};

cipher('3des-cbc') ->
    #cipher_data{key_bytes = 24,
                 iv_bytes = 8,
                 block_bytes = 8};
    
cipher('aes128-cbc') ->
    #cipher_data{key_bytes = 16,
                 iv_bytes = 16,
                 block_bytes = 16};

cipher('aes128-ctr') ->
    #cipher_data{key_bytes = 16,
                 iv_bytes = 16,
                 block_bytes = 16};

cipher('aes192-ctr') ->
    #cipher_data{key_bytes = 24,
                 iv_bytes = 16,
                 block_bytes = 16};

cipher('aes256-ctr') ->
    #cipher_data{key_bytes = 32,
                 iv_bytes = 16,
                 block_bytes = 16}.


encrypt_init(#ssh{encrypt = none} = Ssh) ->
    {ok, Ssh};
encrypt_init(#ssh{encrypt = 'AEAD_AES_128_GCM', role = client} = Ssh) ->
    IV = hash(Ssh, "A", 12*8),
    <<K:16/binary>> = hash(Ssh, "C", 128),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
		 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = 'AEAD_AES_128_GCM', role = server} = Ssh) ->
    IV = hash(Ssh, "B", 12*8),
    <<K:16/binary>> = hash(Ssh, "D", 128),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
		 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = 'AEAD_AES_256_GCM', role = client} = Ssh) ->
    IV = hash(Ssh, "A", 12*8),
    <<K:32/binary>> = hash(Ssh, "C", 256),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
		 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = 'AEAD_AES_256_GCM', role = server} = Ssh) ->
    IV = hash(Ssh, "B", 12*8),
    <<K:32/binary>> = hash(Ssh, "D", 256),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
		 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = '3des-cbc', role = client} = Ssh) ->
    IV = hash(Ssh, "A", 64),
    <<K1:8/binary, K2:8/binary, K3:8/binary>> = hash(Ssh, "C", 192),
    {ok, Ssh#ssh{encrypt_keys = {K1,K2,K3},
		 encrypt_block_size = 8,
		 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = '3des-cbc', role = server} = Ssh) ->
    IV = hash(Ssh, "B", 64),
    <<K1:8/binary, K2:8/binary, K3:8/binary>> = hash(Ssh, "D", 192),
    {ok, Ssh#ssh{encrypt_keys = {K1,K2,K3},
		 encrypt_block_size = 8,
		 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = 'aes128-cbc', role = client} = Ssh) ->
    IV = hash(Ssh, "A", 128),
    <<K:16/binary>> = hash(Ssh, "C", 128),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
		 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = 'aes128-cbc', role = server} = Ssh) ->
    IV = hash(Ssh, "B", 128),
    <<K:16/binary>> = hash(Ssh, "D", 128),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
                 encrypt_ctx = IV}};
encrypt_init(#ssh{encrypt = 'aes128-ctr', role = client} = Ssh) ->
    IV = hash(Ssh, "A", 128),
    <<K:16/binary>> = hash(Ssh, "C", 128),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
                 encrypt_ctx = State}};
encrypt_init(#ssh{encrypt = 'aes192-ctr', role = client} = Ssh) ->
    IV = hash(Ssh, "A", 128),
    <<K:24/binary>> = hash(Ssh, "C", 192),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
                 encrypt_ctx = State}};
encrypt_init(#ssh{encrypt = 'aes256-ctr', role = client} = Ssh) ->
    IV = hash(Ssh, "A", 128),
    <<K:32/binary>> = hash(Ssh, "C", 256),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
                 encrypt_ctx = State}};
encrypt_init(#ssh{encrypt = 'aes128-ctr', role = server} = Ssh) ->
    IV = hash(Ssh, "B", 128),
    <<K:16/binary>> = hash(Ssh, "D", 128),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
                 encrypt_ctx = State}};
encrypt_init(#ssh{encrypt = 'aes192-ctr', role = server} = Ssh) ->
    IV = hash(Ssh, "B", 128),
    <<K:24/binary>> = hash(Ssh, "D", 192),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
                 encrypt_ctx = State}};
encrypt_init(#ssh{encrypt = 'aes256-ctr', role = server} = Ssh) ->
    IV = hash(Ssh, "B", 128),
    <<K:32/binary>> = hash(Ssh, "D", 256),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{encrypt_keys = K,
		 encrypt_block_size = 16,
                 encrypt_ctx = State}}.

encrypt_final(Ssh) ->
    {ok, Ssh#ssh{encrypt = none, 
		 encrypt_keys = undefined,
		 encrypt_block_size = 8,
		 encrypt_ctx = undefined
		}}.

encrypt(#ssh{encrypt = none} = Ssh, Data) ->
    {Ssh, Data};
encrypt(#ssh{encrypt = 'AEAD_AES_128_GCM',
            encrypt_keys = K,
            encrypt_ctx = IV0} = Ssh, Data={_AAD,_Ptext}) ->
    Enc = {_Ctext,_Ctag} = crypto:block_encrypt(aes_gcm, K, IV0, Data),
    IV = next_gcm_iv(IV0),
    {Ssh#ssh{encrypt_ctx = IV}, Enc};
encrypt(#ssh{encrypt = 'AEAD_AES_256_GCM',
            encrypt_keys = K,
            encrypt_ctx = IV0} = Ssh, Data={_AAD,_Ptext}) ->
    Enc = {_Ctext,_Ctag} = crypto:block_encrypt(aes_gcm, K, IV0, Data),
    IV = next_gcm_iv(IV0),
    {Ssh#ssh{encrypt_ctx = IV}, Enc};
encrypt(#ssh{encrypt = '3des-cbc',
	     encrypt_keys = {K1,K2,K3},
	     encrypt_ctx = IV0} = Ssh, Data) ->
    Enc = crypto:block_encrypt(des3_cbc, [K1,K2,K3], IV0, Data),
    IV = crypto:next_iv(des3_cbc, Enc),
    {Ssh#ssh{encrypt_ctx = IV}, Enc};
encrypt(#ssh{encrypt = 'aes128-cbc',
            encrypt_keys = K,
            encrypt_ctx = IV0} = Ssh, Data) ->
    Enc = crypto:block_encrypt(aes_cbc128, K,IV0,Data),
    IV = crypto:next_iv(aes_cbc, Enc),
    {Ssh#ssh{encrypt_ctx = IV}, Enc};
encrypt(#ssh{encrypt = 'aes128-ctr',
            encrypt_ctx = State0} = Ssh, Data) ->
    {State, Enc} = crypto:stream_encrypt(State0,Data),
    {Ssh#ssh{encrypt_ctx = State}, Enc};
encrypt(#ssh{encrypt = 'aes192-ctr',
            encrypt_ctx = State0} = Ssh, Data) ->
    {State, Enc} = crypto:stream_encrypt(State0,Data),
    {Ssh#ssh{encrypt_ctx = State}, Enc};
encrypt(#ssh{encrypt = 'aes256-ctr',
            encrypt_ctx = State0} = Ssh, Data) ->
    {State, Enc} = crypto:stream_encrypt(State0,Data),
    {Ssh#ssh{encrypt_ctx = State}, Enc}.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decryption
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decrypt_init(#ssh{decrypt = none} = Ssh) ->
    {ok, Ssh};
decrypt_init(#ssh{decrypt = 'AEAD_AES_128_GCM', role = client} = Ssh) ->
    IV = hash(Ssh, "B", 12*8),
    <<K:16/binary>> = hash(Ssh, "D", 128),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
		 decrypt_ctx = IV}};
decrypt_init(#ssh{decrypt = 'AEAD_AES_128_GCM', role = server} = Ssh) ->
    IV = hash(Ssh, "A", 12*8),
    <<K:16/binary>> = hash(Ssh, "C", 128),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
		 decrypt_ctx = IV}};
decrypt_init(#ssh{decrypt = 'AEAD_AES_256_GCM', role = client} = Ssh) ->
    IV = hash(Ssh, "B", 12*8),
    <<K:32/binary>> = hash(Ssh, "D", 256),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
		 decrypt_ctx = IV}};
decrypt_init(#ssh{decrypt = 'AEAD_AES_256_GCM', role = server} = Ssh) ->
    IV = hash(Ssh, "A", 12*8),
    <<K:32/binary>> = hash(Ssh, "C", 256),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
		 decrypt_ctx = IV}};
decrypt_init(#ssh{decrypt = '3des-cbc', role = client} = Ssh) ->
    {IV, KD} = {hash(Ssh, "B", 64),
		hash(Ssh, "D", 192)},
    <<K1:8/binary, K2:8/binary, K3:8/binary>> = KD,
    {ok, Ssh#ssh{decrypt_keys = {K1,K2,K3}, decrypt_ctx = IV,
			 decrypt_block_size = 8}}; 
decrypt_init(#ssh{decrypt = '3des-cbc', role = server} = Ssh) ->
    {IV, KD} = {hash(Ssh, "A", 64),
		hash(Ssh, "C", 192)},
    <<K1:8/binary, K2:8/binary, K3:8/binary>> = KD,
    {ok, Ssh#ssh{decrypt_keys = {K1, K2, K3}, decrypt_ctx = IV,
		 decrypt_block_size = 8}};
decrypt_init(#ssh{decrypt = 'aes128-cbc', role = client} = Ssh) ->
    {IV, KD} = {hash(Ssh, "B", 128),
		hash(Ssh, "D", 128)},
    <<K:16/binary>> = KD,
    {ok, Ssh#ssh{decrypt_keys = K, decrypt_ctx = IV,
		 decrypt_block_size = 16}};
decrypt_init(#ssh{decrypt = 'aes128-cbc', role = server} = Ssh) ->
    {IV, KD} = {hash(Ssh, "A", 128),
		hash(Ssh, "C", 128)},
    <<K:16/binary>> = KD,
    {ok, Ssh#ssh{decrypt_keys = K, decrypt_ctx = IV,
		 decrypt_block_size = 16}};
decrypt_init(#ssh{decrypt = 'aes128-ctr', role = client} = Ssh) ->
	IV = hash(Ssh, "B", 128),
    <<K:16/binary>> = hash(Ssh, "D", 128),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
                 decrypt_ctx = State}};
decrypt_init(#ssh{decrypt = 'aes192-ctr', role = client} = Ssh) ->
	IV = hash(Ssh, "B", 128),
    <<K:24/binary>> = hash(Ssh, "D", 192),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
                 decrypt_ctx = State}};
decrypt_init(#ssh{decrypt = 'aes256-ctr', role = client} = Ssh) ->
	IV = hash(Ssh, "B", 128),
    <<K:32/binary>> = hash(Ssh, "D", 256),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
                 decrypt_ctx = State}};
decrypt_init(#ssh{decrypt = 'aes128-ctr', role = server} = Ssh) ->
	IV = hash(Ssh, "A", 128),
    <<K:16/binary>> = hash(Ssh, "C", 128),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
                 decrypt_ctx = State}};
decrypt_init(#ssh{decrypt = 'aes192-ctr', role = server} = Ssh) ->
	IV = hash(Ssh, "A", 128),
    <<K:24/binary>> = hash(Ssh, "C", 192),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
                 decrypt_ctx = State}};
decrypt_init(#ssh{decrypt = 'aes256-ctr', role = server} = Ssh) ->
	IV = hash(Ssh, "A", 128),
    <<K:32/binary>> = hash(Ssh, "C", 256),
    State = crypto:stream_init(aes_ctr, K, IV),
    {ok, Ssh#ssh{decrypt_keys = K,
		 decrypt_block_size = 16,
                 decrypt_ctx = State}}.

  
decrypt_final(Ssh) ->
    {ok, Ssh#ssh {decrypt = none, 
		  decrypt_keys = undefined,
		  decrypt_ctx = undefined,
		  decrypt_block_size = 8}}.

decrypt(Ssh, <<>>) ->
    {Ssh, <<>>};
decrypt(#ssh{decrypt = none} = Ssh, Data) ->
    {Ssh, Data};
decrypt(#ssh{decrypt = 'AEAD_AES_128_GCM',
	     decrypt_keys = K,
	     decrypt_ctx = IV0} = Ssh, Data = {_AAD,_Ctext,_Ctag}) ->
    Dec = crypto:block_decrypt(aes_gcm, K, IV0, Data), % Dec = PlainText | error 
    IV = next_gcm_iv(IV0),
    {Ssh#ssh{decrypt_ctx = IV}, Dec};
decrypt(#ssh{decrypt = 'AEAD_AES_256_GCM',
	     decrypt_keys = K,
	     decrypt_ctx = IV0} = Ssh, Data = {_AAD,_Ctext,_Ctag}) ->
    Dec = crypto:block_decrypt(aes_gcm, K, IV0, Data), % Dec = PlainText | error 
    IV = next_gcm_iv(IV0),
    {Ssh#ssh{decrypt_ctx = IV}, Dec};
decrypt(#ssh{decrypt = '3des-cbc', decrypt_keys = Keys,
	     decrypt_ctx = IV0} = Ssh, Data) ->
    {K1, K2, K3} = Keys,
    Dec = crypto:block_decrypt(des3_cbc, [K1,K2,K3], IV0, Data),
    IV = crypto:next_iv(des3_cbc, Data),
    {Ssh#ssh{decrypt_ctx = IV}, Dec};
decrypt(#ssh{decrypt = 'aes128-cbc', decrypt_keys = Key,
	     decrypt_ctx = IV0} = Ssh, Data) ->
    Dec = crypto:block_decrypt(aes_cbc128, Key,IV0,Data),
    IV = crypto:next_iv(aes_cbc, Data),
    {Ssh#ssh{decrypt_ctx = IV}, Dec};
decrypt(#ssh{decrypt = 'aes128-ctr',
            decrypt_ctx = State0} = Ssh, Data) ->
    {State, Enc} = crypto:stream_decrypt(State0,Data),
    {Ssh#ssh{decrypt_ctx = State}, Enc};
decrypt(#ssh{decrypt = 'aes192-ctr',
            decrypt_ctx = State0} = Ssh, Data) ->
    {State, Enc} = crypto:stream_decrypt(State0,Data),
    {Ssh#ssh{decrypt_ctx = State}, Enc};
decrypt(#ssh{decrypt = 'aes256-ctr',
            decrypt_ctx = State0} = Ssh, Data) ->
    {State, Enc} = crypto:stream_decrypt(State0,Data),
    {Ssh#ssh{decrypt_ctx = State}, Enc}.


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
	aead ->
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
	aead ->
	    %% Not applicable
	    {ok, SSH}
    end.

recv_mac_final(SSH) ->
    {ok, SSH#ssh { recv_mac = none, recv_mac_key = undefined }}.

mac(none, _ , _, _) ->
    <<>>;
mac('hmac-sha1', Key, SeqNum, Data) ->
    crypto:hmac(sha, Key, [<<?UINT32(SeqNum)>>, Data]);
mac('hmac-sha1-96', Key, SeqNum, Data) ->
    crypto:hmac(sha, Key, [<<?UINT32(SeqNum)>>, Data], mac_digest_size('hmac-sha1-96'));
mac('hmac-md5', Key, SeqNum, Data) ->
    crypto:hmac(md5, Key, [<<?UINT32(SeqNum)>>, Data]);
mac('hmac-md5-96', Key, SeqNum, Data) ->
    crypto:hmac(md5, Key, [<<?UINT32(SeqNum)>>, Data], mac_digest_size('hmac-md5-96'));
mac('hmac-sha2-256', Key, SeqNum, Data) ->
	crypto:hmac(sha256, Key, [<<?UINT32(SeqNum)>>, Data]);
mac('hmac-sha2-512', Key, SeqNum, Data) ->
	crypto:hmac(sha512, Key, [<<?UINT32(SeqNum)>>, Data]).


%%%----------------------------------------------------------------
%% return N hash bytes (HASH)
hash(_SSH, _Char, 0) ->
    <<>>;
hash(SSH, Char, N) ->
    HashAlg = sha(SSH#ssh.kex),
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
kex_hash(SSH, Key, SignAlg, HashAlg, Args) ->
    crypto:hash(HashAlg, kex_plaintext(SSH,Key,SignAlg,Args)).

kex_plaintext(SSH, Key, SignAlg, Args) ->
    EncodedKey = public_key:ssh_encode({Key,SignAlg}, ssh2_pubkey),
    <<?Estring(SSH#ssh.c_version), ?Estring(SSH#ssh.s_version),
      ?Ebinary(SSH#ssh.c_keyinit), ?Ebinary(SSH#ssh.s_keyinit),
      ?Ebinary(EncodedKey),
      (kex_alg_dependent(Args))/binary>>.

kex_alg_dependent({E, F, K}) ->
    %% diffie-hellman and ec diffie-hellman (with E = Q_c, F = Q_s)
    <<?Empint(E), ?Empint(F), ?Empint(K)>>;

kex_alg_dependent({-1, _, -1, _, _, E, F, K}) ->
    %% ssh_msg_kex_dh_gex_request_old
    <<?Empint(E), ?Empint(F), ?Empint(K)>>;

kex_alg_dependent({Min, NBits, Max, Prime, Gen, E, F, K}) ->
    %% diffie-hellman group exchange
    <<?Euint32(Min), ?Euint32(NBits), ?Euint32(Max),
      ?Empint(Prime), ?Empint(Gen), ?Empint(E), ?Empint(F), ?Empint(K)>>.

%%%----------------------------------------------------------------

valid_key_sha_alg(#'RSAPublicKey'{}, 'rsa-sha2-512') -> true;
valid_key_sha_alg(#'RSAPublicKey'{}, 'rsa-sha2-384') -> true;
valid_key_sha_alg(#'RSAPublicKey'{}, 'rsa-sha2-256') -> true;
valid_key_sha_alg(#'RSAPublicKey'{}, 'ssh-rsa'     ) -> true;

valid_key_sha_alg(#'RSAPrivateKey'{}, 'rsa-sha2-512') -> true;
valid_key_sha_alg(#'RSAPrivateKey'{}, 'rsa-sha2-384') -> true;
valid_key_sha_alg(#'RSAPrivateKey'{}, 'rsa-sha2-256') -> true;
valid_key_sha_alg(#'RSAPrivateKey'{}, 'ssh-rsa'     ) -> true;

valid_key_sha_alg({_, #'Dss-Parms'{}}, 'ssh-dss') -> true;
valid_key_sha_alg(#'DSAPrivateKey'{},  'ssh-dss') -> true;

valid_key_sha_alg({#'ECPoint'{},{namedCurve,OID}},                Alg) -> sha(OID) == sha(Alg);
valid_key_sha_alg(#'ECPrivateKey'{parameters = {namedCurve,OID}}, Alg) -> sha(OID) == sha(Alg);
valid_key_sha_alg(_, _) -> false.
    


public_algo(#'RSAPublicKey'{}) ->   'ssh-rsa';  % FIXME: Not right with draft-curdle-rsa-sha2
public_algo({_, #'Dss-Parms'{}}) -> 'ssh-dss';
public_algo({#'ECPoint'{},{namedCurve,OID}}) -> 
    Curve = public_key:oid2ssh_curvename(OID),
    list_to_atom("ecdsa-sha2-" ++ binary_to_list(Curve)).




sha('ssh-rsa') -> sha;
sha('rsa-sha2-256') -> sha256;
sha('rsa-sha2-384') -> sha384;
sha('rsa-sha2-512') -> sha512;
sha('ssh-dss') -> sha;
sha('ecdsa-sha2-nistp256') -> sha(secp256r1);
sha('ecdsa-sha2-nistp384') -> sha(secp384r1);
sha('ecdsa-sha2-nistp521') -> sha(secp521r1);
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
sha(Str) when is_list(Str), length(Str)<50 -> sha(list_to_atom(Str)).


mac_key_bytes('hmac-sha1')    -> 20;
mac_key_bytes('hmac-sha1-96') -> 20;
mac_key_bytes('hmac-md5')     -> 16;
mac_key_bytes('hmac-md5-96')  -> 16;
mac_key_bytes('hmac-sha2-256')-> 32;
mac_key_bytes('hmac-sha2-512')-> 64;
mac_key_bytes('AEAD_AES_128_GCM') -> 0;
mac_key_bytes('AEAD_AES_256_GCM') -> 0;
mac_key_bytes(none) -> 0.

mac_digest_size('hmac-sha1')    -> 20;
mac_digest_size('hmac-sha1-96') -> 12;
mac_digest_size('hmac-md5')    -> 20;
mac_digest_size('hmac-md5-96') -> 12;
mac_digest_size('hmac-sha2-256') -> 32;
mac_digest_size('hmac-sha2-512') -> 64;
mac_digest_size('AEAD_AES_128_GCM') -> 16;
mac_digest_size('AEAD_AES_256_GCM') -> 16;
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


generate_key(Algorithm, Args) ->
    {Public,Private} = crypto:generate_key(Algorithm, Args),
    {crypto:bytes_to_integer(Public), crypto:bytes_to_integer(Private)}.

      
compute_key(Algorithm, OthersPublic, MyPrivate, Args) ->
    Shared = crypto:compute_key(Algorithm, OthersPublic, MyPrivate, Args),
    crypto:bytes_to_integer(Shared).


dh_bits(#alg{encrypt = Encrypt,
             send_mac = SendMac}) ->
    C = cipher(Encrypt),
    8 * lists:max([C#cipher_data.key_bytes,
                   C#cipher_data.block_bytes,
                   C#cipher_data.iv_bytes,
                   mac_key_bytes(SendMac)
                  ]).

ecdh_curve('ecdh-sha2-nistp256') -> secp256r1;
ecdh_curve('ecdh-sha2-nistp384') -> secp384r1;
ecdh_curve('ecdh-sha2-nistp521') -> secp521r1.


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
    Sup = [{ec_curve,crypto_supported_curves()} | crypto:supports()],
    [Name || {Name,CryptoRequires} <- L,
	     crypto_supported(CryptoRequires, Sup)].

crypto_supported_curves() ->
    try crypto:ec_curves()
    catch _:_ -> []
    end.

crypto_supported(Conditions, Supported) ->
    lists:all( fun({Tag,CryptoName}) when is_atom(CryptoName) ->
		       crypto_name_supported(Tag,CryptoName,Supported);
		  ({Tag,{Name,Len}}) when is_integer(Len) ->
		       crypto_name_supported(Tag,Name,Supported) andalso
			   len_supported(Name,Len)
	       end, Conditions).

crypto_name_supported(Tag, CryptoName, Supported) ->
    lists:member(CryptoName, proplists:get_value(Tag,Supported,[])).

len_supported(Name, Len) ->
    try
	case Name of
	    aes_ctr ->
		{_, <<_/binary>>} = 
		    %% Test encryption
		    crypto:stream_encrypt(crypto:stream_init(Name, <<0:Len>>, <<0:128>>), <<"">>);
	    aes_gcm ->
		{<<_/binary>>, <<_/binary>>} = 
		    crypto:block_encrypt(Name, 
					 _Key = <<0:Len>>,
					 _IV = <<0:12/unsigned-unit:8>>,
					 {<<"AAD">>,"PT"})
	end
    of
	_ -> true
    catch
	_:_ -> false
    end.
	    

same(Algs) ->  [{client2server,Algs}, {server2client,Algs}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Other utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trim_tail(Str) ->
    lists:reverse(trim_head(lists:reverse(Str))).

trim_head([$\s|Cs]) -> trim_head(Cs);
trim_head([$\t|Cs]) -> trim_head(Cs);
trim_head([$\n|Cs]) -> trim_head(Cs);
trim_head([$\r|Cs]) -> trim_head(Cs);
trim_head(Cs) -> Cs.


