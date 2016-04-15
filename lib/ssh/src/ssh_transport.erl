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
	 handle_kexinit_msg/3, handle_kexdh_init/2,
	 handle_kex_dh_gex_group/2, handle_kex_dh_gex_init/2, handle_kex_dh_gex_reply/2,
	 handle_new_keys/2, handle_kex_dh_gex_request/2,
	 handle_kexdh_reply/2, 
	 handle_kex_ecdh_init/2,
	 handle_kex_ecdh_reply/2,
	 extract_public_key/1,
	 ssh_packet/2, pack/2,
	 sign/3, verify/4]).

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


default_algorithms(cipher) ->
    supported_algorithms(cipher, same(['AEAD_AES_128_GCM',
				       'AEAD_AES_256_GCM']));
default_algorithms(mac) ->
    supported_algorithms(mac, same(['AEAD_AES_128_GCM',
				    'AEAD_AES_256_GCM']));
default_algorithms(Alg) ->
    supported_algorithms(Alg, []).


supported_algorithms() -> [{K,supported_algorithms(K)} || K <- algo_classes()].

supported_algorithms(kex) ->
    select_crypto_supported(
      [
       {'ecdh-sha2-nistp256',                   [{public_keys,ecdh}, {ec_curve,secp256r1}, {hashs,sha256}]},
       {'ecdh-sha2-nistp384',                   [{public_keys,ecdh}, {ec_curve,secp384r1}, {hashs,sha384}]},
       {'diffie-hellman-group14-sha1',          [{public_keys,dh},   {hashs,sha}]},
       {'diffie-hellman-group-exchange-sha256', [{public_keys,dh},   {hashs,sha256}]},
       {'diffie-hellman-group-exchange-sha1',   [{public_keys,dh},   {hashs,sha}]},
       {'ecdh-sha2-nistp521',                   [{public_keys,ecdh}, {ec_curve,secp521r1}, {hashs,sha512}]},
       {'diffie-hellman-group1-sha1',           [{public_keys,dh},   {hashs,sha}]}
      ]);
supported_algorithms(public_key) ->
    select_crypto_supported(
      [{'ecdsa-sha2-nistp256',  [{public_keys,ecdsa}, {hashs,sha256}, {ec_curve,secp256r1}]},
       {'ecdsa-sha2-nistp384',  [{public_keys,ecdsa}, {hashs,sha384}, {ec_curve,secp384r1}]},
       {'ecdsa-sha2-nistp521',  [{public_keys,ecdsa}, {hashs,sha512}, {ec_curve,secp521r1}]},
       {'ssh-rsa',              [{public_keys,rsa},   {hashs,sha}                         ]},
       {'ssh-dss',              [{public_keys,dss},   {hashs,sha}                         ]}
      ]);
 
supported_algorithms(cipher) ->
    same(
      select_crypto_supported(
	[{'aes256-ctr',       [{ciphers,{aes_ctr,256}}]},
	 {'aes192-ctr',       [{ciphers,{aes_ctr,192}}]},
	 {'aes128-ctr',       [{ciphers,{aes_ctr,128}}]},
	 {'aes128-cbc',       [{ciphers,aes_cbc128}]},
	 {'aes128-gcm@openssh.com', [{ciphers,{aes_gcm,128}}]},
	 {'aes256-gcm@openssh.com', [{ciphers,{aes_gcm,256}}]},
	 {'AEAD_AES_128_GCM', [{ciphers,{aes_gcm,128}}]},
	 {'AEAD_AES_256_GCM', [{ciphers,{aes_gcm,256}}]},
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
    Vsn = proplists:get_value(vsn, Options, ?DEFAULT_CLIENT_VERSION),
    {Vsn, format_version(Vsn, software_version(Options))};
versions(server, Options) ->
    Vsn = proplists:get_value(vsn, Options, ?DEFAULT_SERVER_VERSION),
    {Vsn, format_version(Vsn, software_version(Options))}.

software_version(Options) -> 
    case proplists:get_value(id_string, Options) of
	undefined ->
	    "Erlang"++ssh_vsn();
	{random,Nlo,Nup} ->
	    random_id(Nlo,Nup);
	ID ->
	    ID
    end.

ssh_vsn() ->
    try {ok,L} = application:get_all_key(ssh),
	 proplists:get_value(vsn,L,"")
    of 
	"" -> "";
	VSN when is_list(VSN) -> "/" ++ VSN;
	_ -> ""
    catch
	_:_ -> ""
    end.
    
random_id(Nlo, Nup) ->
    [crypto:rand_uniform($a,$z+1) || _<- lists:duplicate(crypto:rand_uniform(Nlo,Nup+1),x)  ].

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

yes_no(Ssh, Prompt)  ->
    (Ssh#ssh.io_cb):yes_no(Prompt, Ssh).

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
    PrefAlgs =
	case proplists:get_value(preferred_algorithms,Opts) of
	    undefined -> 
		default_algorithms();
	    Algs0 ->
		Algs0
	end,
    kexinit_message(Role, Random, PrefAlgs, HostKeyAlgs).

key_init(client, Ssh, Value) ->
    Ssh#ssh{c_keyinit = Value};
key_init(server, Ssh, Value) ->
    Ssh#ssh{s_keyinit = Value}.


kexinit_message(_Role, Random, Algs, HostKeyAlgs) ->
    #ssh_msg_kexinit{
		  cookie = Random,
		  kex_algorithms = to_strings( get_algs(kex,Algs) ),
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
    {SshPacket, Ssh} = 
	ssh_packet(#ssh_msg_newkeys{}, Ssh0),
    {ok, SshPacket, Ssh}.


handle_kexinit_msg(#ssh_msg_kexinit{} = CounterPart, #ssh_msg_kexinit{} = Own,
			    #ssh{role = client} = Ssh0) ->
    {ok, Algoritms} = select_algorithm(client, Own, CounterPart),  
    case verify_algorithm(Algoritms) of
	true ->
	    key_exchange_first_msg(Algoritms#alg.kex, 
				   Ssh0#ssh{algorithms = Algoritms});
	_  ->
	    %% TODO: Correct code?
    ssh_connection_handler:disconnect(
      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			  description = "Selection of key exchange algorithm failed"
			 })
    end;

handle_kexinit_msg(#ssh_msg_kexinit{} = CounterPart, #ssh_msg_kexinit{} = Own,
			    #ssh{role = server} = Ssh) ->
    {ok, Algoritms} = select_algorithm(server, CounterPart, Own),
    case  verify_algorithm(Algoritms) of
	true ->
	    {ok, Ssh#ssh{algorithms = Algoritms}};
	_ ->
    ssh_connection_handler:disconnect(
      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			  description = "Selection of key exchange algorithm failed"
			 })
    end.


%% TODO: diffie-hellman-group14-sha1 should also be supported.
%% Maybe check more things ...

verify_algorithm(#alg{kex = undefined}) -> false;
verify_algorithm(#alg{hkey = undefined}) -> false;
verify_algorithm(#alg{send_mac = undefined}) -> false;
verify_algorithm(#alg{recv_mac = undefined}) -> false;
verify_algorithm(#alg{encrypt = undefined}) -> false;
verify_algorithm(#alg{decrypt = undefined}) -> false;
verify_algorithm(#alg{compress = undefined}) -> false;
verify_algorithm(#alg{decompress = undefined}) -> false;
verify_algorithm(#alg{kex = Kex}) -> lists:member(Kex, supported_algorithms(kex)).

%%%----------------------------------------------------------------
%%%
%%% Key exchange initialization
%%%
key_exchange_first_msg(Kex, Ssh0) when Kex == 'diffie-hellman-group1-sha1' ;
				       Kex == 'diffie-hellman-group14-sha1' ->
    {G, P} = dh_group(Kex),
    {Public, Private} = generate_key(dh, [P,G]),
    {SshPacket, Ssh1} = ssh_packet(#ssh_msg_kexdh_init{e = Public}, Ssh0),
    {ok, SshPacket, 
     Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}}}};

key_exchange_first_msg(Kex, Ssh0=#ssh{opts=Opts}) when Kex == 'diffie-hellman-group-exchange-sha1' ;
						       Kex == 'diffie-hellman-group-exchange-sha256' ->
    {Min,NBits,Max} = 
	proplists:get_value(dh_gex_limits, Opts, {?DEFAULT_DH_GROUP_MIN,
						  ?DEFAULT_DH_GROUP_NBITS,
						  ?DEFAULT_DH_GROUP_MAX}),
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
%%% 
handle_kexdh_init(#ssh_msg_kexdh_init{e = E}, 
		  Ssh0 = #ssh{algorithms = #alg{kex=Kex}}) ->
    %% server
    {G, P} = dh_group(Kex),
    if
	1=<E, E=<(P-1) ->
	    {Public, Private} = generate_key(dh, [P,G]),
	    K = compute_key(dh, E, Private, [P,G]),
	    MyPrivHostKey = get_host_key(Ssh0),
	    MyPubHostKey = extract_public_key(MyPrivHostKey),
	    H = kex_h(Ssh0, MyPubHostKey, E, Public, K),
	    H_SIG = sign_host_key(Ssh0, MyPrivHostKey, H),
	    {SshPacket, Ssh1} = 
		ssh_packet(#ssh_msg_kexdh_reply{public_host_key = MyPubHostKey,
						f = Public,
						h_sig = H_SIG
					       }, Ssh0),
	    {ok, SshPacket, Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}},
				     shared_secret = K,
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
		   #ssh{keyex_key = {{Private, Public}, {G, P}}} = Ssh0) ->
    %% client
    if 
	1=<F, F=<(P-1)->
	    K = compute_key(dh, F, Private, [P,G]),
	    H = kex_h(Ssh0, PeerPubHostKey, Public, F, K),

	    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
		ok ->
		    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
		    {ok, SshPacket, Ssh#ssh{shared_secret  = K,
					    exchanged_hash = H,
					    session_id = sid(Ssh, H)}};
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
				 proplists:get_value(dh_gex_groups,Opts)) of
	{ok, {_Sz, {G,P}}} ->
	    {Public, Private} = generate_key(dh, [P,G]),
	    {SshPacket, Ssh} = 
		ssh_packet(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0),
	    {ok, SshPacket, 
	     Ssh#ssh{keyex_key = {{Private, Public}, {G, P}},
		     keyex_info = {Min, Max, NBits}
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
    %% Unfortunatly the rfc does not specify how to treat the parameter n
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
				 proplists:get_value(dh_gex_groups,Opts)) of
	{ok, {_Sz, {G,P}}} ->
	    {Public, Private} = generate_key(dh, [P,G]),
	    {SshPacket, Ssh} = 
		ssh_packet(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0),
	    {ok, SshPacket, 
	     Ssh#ssh{keyex_key = {{Private, Public}, {G, P}},
		     keyex_info = {-1, -1, NBits} % flag for kex_h hash calc
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
    case proplists:get_value(dh_gex_limits, Opts) of
	undefined ->
	    {Min0, Max0};
	{Min1, Max1} ->
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
	    end
    end.
		    

handle_kex_dh_gex_group(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0) ->
    %% client
    {Public, Private} = generate_key(dh, [P,G]),
    {SshPacket, Ssh1} = 
	ssh_packet(#ssh_msg_kex_dh_gex_init{e = Public}, Ssh0),	% Pub = G^Priv mod P (def)

    {ok, SshPacket, 
     Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}}}}.

handle_kex_dh_gex_init(#ssh_msg_kex_dh_gex_init{e = E}, 
		       #ssh{keyex_key = {{Private, Public}, {G, P}},
			    keyex_info = {Min, Max, NBits}} = 
			   Ssh0) ->
    %% server
    if
	1=<E, E=<(P-1) ->
	    K = compute_key(dh, E, Private, [P,G]),
	    if
		1<K, K<(P-1) ->
		    MyPrivHostKey = get_host_key(Ssh0),
		    MyPubHostKey = extract_public_key(MyPrivHostKey),
		    H = kex_h(Ssh0, MyPubHostKey, Min, NBits, Max, P, G, E, Public, K),
		    H_SIG = sign_host_key(Ssh0, MyPrivHostKey, H),
		    {SshPacket, Ssh} = 
			ssh_packet(#ssh_msg_kex_dh_gex_reply{public_host_key = MyPubHostKey,
							     f = Public,
							     h_sig = H_SIG}, Ssh0),
		    {ok, SshPacket, Ssh#ssh{shared_secret = K,
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
			     keyex_info = {Min, Max, NBits}} = 
			    Ssh0) ->
    %% client
    if 
	1=<F, F=<(P-1)->
	    K = compute_key(dh, F, Private, [P,G]),
	    if
		1<K, K<(P-1) ->
		    H = kex_h(Ssh0, PeerPubHostKey, Min, NBits, Max, P, G, Public, F, K),

		    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
			ok ->
			    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
			    {ok, SshPacket, Ssh#ssh{shared_secret  = K,
						    exchanged_hash = H,
						    session_id = sid(Ssh, H)}};
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
		     Ssh0 = #ssh{algorithms = #alg{kex=Kex}}) ->
    %% at server
    Curve = ecdh_curve(Kex),
    {MyPublic, MyPrivate} = generate_key(ecdh, Curve),
    try
	compute_key(ecdh, PeerPublic, MyPrivate, Curve)
    of
	K ->
	    MyPrivHostKey = get_host_key(Ssh0),
	    MyPubHostKey = extract_public_key(MyPrivHostKey),
	    H = kex_h(Ssh0, Curve, MyPubHostKey, PeerPublic, MyPublic, K),
	    H_SIG = sign_host_key(Ssh0, MyPrivHostKey, H),
	    {SshPacket, Ssh1} = 
		ssh_packet(#ssh_msg_kex_ecdh_reply{public_host_key = MyPubHostKey,
						   q_s = MyPublic,
						   h_sig = H_SIG},
			   Ssh0),
    	    {ok, SshPacket, Ssh1#ssh{keyex_key = {{MyPublic,MyPrivate},Curve},
				     shared_secret = K,
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
		      #ssh{keyex_key = {{MyPublic,MyPrivate}, Curve}} = Ssh0
		     ) ->
    %% at client
    try
	compute_key(ecdh, PeerPublic, MyPrivate, Curve)
    of
	K ->
	    H = kex_h(Ssh0, Curve, PeerPubHostKey, MyPublic, PeerPublic, K), 
	    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
		ok ->
		    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
		    {ok, SshPacket, Ssh#ssh{shared_secret  = K,
					    exchanged_hash = H,
					    session_id = sid(Ssh, H)}};
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
    try install_alg(Ssh0) of
	#ssh{} = Ssh ->
	    {ok, Ssh}
    catch 
	_C:_Error -> %% TODO: Throw earlier ....
	    ssh_connection_handler:disconnect(
	      #ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				  description = "Install alg failed"
				 })
    end. 

%% select session id
sid(#ssh{session_id = undefined}, H) -> 
    H;
sid(#ssh{session_id = Id}, _) -> 
    Id.

%%
%% The host key should be read from storage
%%
get_host_key(SSH) ->
    #ssh{key_cb = Mod, opts = Opts, algorithms = ALG} = SSH,

    case Mod:host_key(ALG#alg.hkey, Opts) of
	{ok, #'RSAPrivateKey'{} = Key} ->  Key;
	{ok, #'DSAPrivateKey'{} = Key} ->  Key;
	{ok, #'ECPrivateKey'{}  = Key} ->  Key;
	Result ->
	    exit({error, {Result, unsupported_key_type}})
    end.

sign_host_key(_Ssh, PrivateKey, H) ->
     sign(H, sign_host_key_sha(PrivateKey), PrivateKey).

sign_host_key_sha(#'ECPrivateKey'{parameters = {namedCurve,OID}}) -> sha(OID);
sign_host_key_sha(#'RSAPrivateKey'{}) -> sha;
sign_host_key_sha(#'DSAPrivateKey'{}) -> sha.


extract_public_key(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #'RSAPublicKey'{modulus = N, publicExponent = E};
extract_public_key(#'DSAPrivateKey'{y = Y, p = P, q = Q, g = G}) ->
    {Y,  #'Dss-Parms'{p=P, q=Q, g=G}};
extract_public_key(#'ECPrivateKey'{parameters = {namedCurve,OID},
				   publicKey = Q}) ->
    {#'ECPoint'{point=Q}, {namedCurve,OID}}.


verify_host_key(SSH, PublicKey, Digest, Signature) ->
    case verify(Digest, host_key_sha(PublicKey), Signature, PublicKey) of
	false ->
	    {error, bad_signature};
	true ->
	    known_host_key(SSH, PublicKey, public_algo(PublicKey))
    end.


host_key_sha(#'RSAPublicKey'{})    -> sha;
host_key_sha({_, #'Dss-Parms'{}})  -> sha;
host_key_sha({#'ECPoint'{},{namedCurve,OID}}) -> sha(OID).

public_algo(#'RSAPublicKey'{}) ->   'ssh-rsa';
public_algo({_, #'Dss-Parms'{}}) -> 'ssh-dss';
public_algo({#'ECPoint'{},{namedCurve,OID}}) -> 
    Curve = public_key:oid2ssh_curvename(OID),
    list_to_atom("ecdsa-sha2-" ++ binary_to_list(Curve)).


accepted_host(Ssh, PeerName, Opts) ->
    case proplists:get_value(silently_accept_hosts, Opts, false) of
	true ->
	    yes;
	false ->
	    yes_no(Ssh, "New host " ++ PeerName ++ " accept")
    end.

known_host_key(#ssh{opts = Opts, key_cb = Mod, peer = Peer} = Ssh, 
	       Public, Alg) ->
    PeerName = peer_name(Peer),
    case Mod:is_host_key(Public, PeerName, Alg, Opts) of
	true ->
	    ok;
	false ->
	    case accepted_host(Ssh, PeerName, Opts) of
		yes ->
		    Mod:add_host_key(PeerName, Public, Opts);
		no ->
		    {error, rejected}
	    end
    end.
	    

%%   Each of the algorithm strings MUST be a comma-separated list of
%%   algorithm names (see ''Algorithm Naming'' in [SSH-ARCH]).  Each
%%   supported (allowed) algorithm MUST be listed in order of preference.
%%
%%   The first algorithm in each list MUST be the preferred (guessed)
%%   algorithm.  Each string MUST contain at least one algorithm name.
select_algorithm(Role, Client, Server) ->
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

    Alg = #alg{kex = Kex, 
	       hkey = HK,
	       encrypt = Encrypt,
	       decrypt = Decrypt,
	       send_mac = SendMac,
	       recv_mac = RecvMac,
	       compress = Compression,
	       decompress = Decompression,
	       c_lng = C_Lng,
	       s_lng = S_Lng},
    {ok, Alg}.


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

install_alg(SSH) ->
    SSH1 = alg_final(SSH),
    SSH2 = alg_setup(SSH1),
    alg_init(SSH2).

alg_setup(SSH) ->
    ALG = SSH#ssh.algorithms,
    SSH#ssh{kex = ALG#alg.kex,
	    hkey = ALG#alg.hkey,
	    encrypt = ALG#alg.encrypt,
	    decrypt = ALG#alg.decrypt,
	    send_mac = ALG#alg.send_mac,
	    send_mac_size = mac_digest_size(ALG#alg.send_mac),
	    recv_mac = ALG#alg.recv_mac,
	    recv_mac_size = mac_digest_size(ALG#alg.recv_mac),
	    compress = ALG#alg.compress,
	    decompress = ALG#alg.decompress,
	    c_lng = ALG#alg.c_lng,
	    s_lng = ALG#alg.s_lng,
	    algorithms = undefined
	   }.

alg_init(SSH0) ->
    {ok,SSH1} = send_mac_init(SSH0),
    {ok,SSH2} = recv_mac_init(SSH1),
    {ok,SSH3} = encrypt_init(SSH2),
    {ok,SSH4} = decrypt_init(SSH3),
    {ok,SSH5} = compress_init(SSH4),
    {ok,SSH6} = decompress_init(SSH5),
    SSH6.

alg_final(SSH0) ->
    {ok,SSH1} = send_mac_final(SSH0),
    {ok,SSH2} = recv_mac_final(SSH1),
    {ok,SSH3} = encrypt_final(SSH2),
    {ok,SSH4} = decrypt_final(SSH3),
    {ok,SSH5} = compress_final(SSH4),
    {ok,SSH6} = decompress_final(SSH5),
    SSH6.

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
    ExtraPaddingLen = try crypto:rand_uniform(0,MaxExtraBlocks)*PadBlockSize
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

sign(SigData, Hash,  #'DSAPrivateKey'{} = Key) ->
    DerSignature = public_key:sign(SigData, Hash, Key),
    #'Dss-Sig-Value'{r = R, s = S} = public_key:der_decode('Dss-Sig-Value', DerSignature),
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>>;
sign(SigData, Hash, Key = #'ECPrivateKey'{}) ->
    DerEncodedSign =  public_key:sign(SigData, Hash, Key),
    #'ECDSA-Sig-Value'{r=R, s=S} = public_key:der_decode('ECDSA-Sig-Value', DerEncodedSign),
    <<?Empint(R),?Empint(S)>>;
sign(SigData, Hash, Key) ->
    public_key:sign(SigData, Hash, Key).

verify(PlainText, Hash, Sig, {_,  #'Dss-Parms'{}} = Key) ->
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>> = Sig,
    Signature = public_key:der_encode('Dss-Sig-Value', #'Dss-Sig-Value'{r = R, s = S}),
    public_key:verify(PlainText, Hash, Signature, Key);
verify(PlainText, Hash, Sig, {#'ECPoint'{},_} = Key) ->
    <<?UINT32(Rlen),R:Rlen/big-signed-integer-unit:8,
      ?UINT32(Slen),S:Slen/big-signed-integer-unit:8>> = Sig,
    Sval = #'ECDSA-Sig-Value'{r=R, s=S},
    DerEncodedSig = public_key:der_encode('ECDSA-Sig-Value',Sval),
    public_key:verify(PlainText, Hash, DerEncodedSig, Key);
verify(PlainText, Hash, Sig, Key) ->
    public_key:verify(PlainText, Hash, Sig, Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Encryption
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
		    KeySize = mac_key_size(SSH#ssh.send_mac),
		    Key = hash(SSH, "E", KeySize),
		    {ok, SSH#ssh { send_mac_key = Key }};
		server ->
		    KeySize = mac_key_size(SSH#ssh.send_mac),
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
		    Key = hash(SSH, "F", mac_key_size(SSH#ssh.recv_mac)),
		    {ok, SSH#ssh { recv_mac_key = Key }};
		server ->
		    Key = hash(SSH, "E", mac_key_size(SSH#ssh.recv_mac)),
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

%% return N hash bytes (HASH)
hash(SSH, Char, Bits) ->
    HASH =
	case SSH#ssh.kex of
	    'diffie-hellman-group1-sha1' ->
		fun(Data) -> crypto:hash(sha, Data) end;
	    'diffie-hellman-group14-sha1' ->
		fun(Data) -> crypto:hash(sha, Data) end;

	    'diffie-hellman-group-exchange-sha1' ->
		fun(Data) -> crypto:hash(sha, Data) end;
	    'diffie-hellman-group-exchange-sha256' ->
		fun(Data) -> crypto:hash(sha256, Data) end;

	    'ecdh-sha2-nistp256' ->
		fun(Data) -> crypto:hash(sha256,Data) end;
	    'ecdh-sha2-nistp384' ->
		fun(Data) -> crypto:hash(sha384,Data) end;
	    'ecdh-sha2-nistp521' ->
		fun(Data) -> crypto:hash(sha512,Data) end;
	    _ ->
		exit({bad_algorithm,SSH#ssh.kex})
	end,
    hash(SSH, Char, Bits, HASH).

hash(_SSH, _Char, 0, _HASH) ->
    <<>>;
hash(SSH, Char, N, HASH) ->
    K = ssh_bits:mpint(SSH#ssh.shared_secret),
    H = SSH#ssh.exchanged_hash,
    SessionID = SSH#ssh.session_id,
    K1 = HASH([K, H, Char, SessionID]),
    Sz = N div 8,
    <<Key:Sz/binary, _/binary>> = hash(K, H, K1, N-128, HASH),
    Key.

hash(_K, _H, Ki, N, _HASH) when N =< 0 ->
    Ki;
hash(K, H, Ki, N, HASH) ->
    Kj = HASH([K, H, Ki]),
    hash(K, H, <<Ki/binary, Kj/binary>>, N-128, HASH).

kex_h(SSH, Key, E, F, K) ->
    KeyBin = public_key:ssh_encode(Key, ssh2_pubkey),
    L = <<?Estring(SSH#ssh.c_version), ?Estring(SSH#ssh.s_version),
	  ?Ebinary(SSH#ssh.c_keyinit), ?Ebinary(SSH#ssh.s_keyinit), ?Ebinary(KeyBin),
	  ?Empint(E), ?Empint(F), ?Empint(K)>>,
    crypto:hash(sha((SSH#ssh.algorithms)#alg.kex), L).

kex_h(SSH, Curve, Key, Q_c, Q_s, K) ->
    KeyBin = public_key:ssh_encode(Key, ssh2_pubkey),
    L = <<?Estring(SSH#ssh.c_version), ?Estring(SSH#ssh.s_version),
	  ?Ebinary(SSH#ssh.c_keyinit), ?Ebinary(SSH#ssh.s_keyinit), ?Ebinary(KeyBin),
	  ?Empint(Q_c), ?Empint(Q_s), ?Empint(K)>>,
    crypto:hash(sha(Curve), L).

kex_h(SSH, Key, Min, NBits, Max, Prime, Gen, E, F, K) ->
    KeyBin = public_key:ssh_encode(Key, ssh2_pubkey),
    L = if Min==-1; Max==-1 ->
		%% flag from 'ssh_msg_kex_dh_gex_request_old'
		%% It was like this before that message was supported,
		%% why?
		<<?Estring(SSH#ssh.c_version), ?Estring(SSH#ssh.s_version),
		  ?Ebinary(SSH#ssh.c_keyinit), ?Ebinary(SSH#ssh.s_keyinit), ?Ebinary(KeyBin),
		  ?Empint(E), ?Empint(F), ?Empint(K)>>;
	   true ->
		<<?Estring(SSH#ssh.c_version), ?Estring(SSH#ssh.s_version),
		  ?Ebinary(SSH#ssh.c_keyinit), ?Ebinary(SSH#ssh.s_keyinit), ?Ebinary(KeyBin),
		  ?Euint32(Min), ?Euint32(NBits), ?Euint32(Max),
		  ?Empint(Prime), ?Empint(Gen), ?Empint(E), ?Empint(F), ?Empint(K)>>
	end,
    crypto:hash(sha((SSH#ssh.algorithms)#alg.kex), L).
  

sha(secp256r1) -> sha256;
sha(secp384r1) -> sha384;
sha(secp521r1) -> sha512;
sha('diffie-hellman-group1-sha1') -> sha;
sha('diffie-hellman-group14-sha1') -> sha;
sha('diffie-hellman-group-exchange-sha1')   -> sha;
sha('diffie-hellman-group-exchange-sha256') -> sha256;
sha(?'secp256r1') -> sha(secp256r1);
sha(?'secp384r1') -> sha(secp384r1);
sha(?'secp521r1') -> sha(secp521r1).


mac_key_size('hmac-sha1')    -> 20*8;
mac_key_size('hmac-sha1-96') -> 20*8;
mac_key_size('hmac-md5')     -> 16*8;
mac_key_size('hmac-md5-96')  -> 16*8;
mac_key_size('hmac-sha2-256')-> 32*8;
mac_key_size('hmac-sha2-512')-> 512;
mac_key_size(none) -> 0.

mac_digest_size('hmac-sha1')    -> 20;
mac_digest_size('hmac-sha1-96') -> 12;
mac_digest_size('hmac-md5')    -> 20;
mac_digest_size('hmac-md5-96') -> 12;
mac_digest_size('hmac-sha2-256') -> 32;
mac_digest_size('hmac-sha2-512') -> 64;
mac_digest_size('AEAD_AES_128_GCM') -> 16;
mac_digest_size('AEAD_AES_256_GCM') -> 16;
mac_digest_size(none) -> 0.

peer_name({Host, _}) ->
    Host.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Diffie-Hellman utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dh_group('diffie-hellman-group1-sha1') ->  ?dh_group1;
dh_group('diffie-hellman-group14-sha1') -> ?dh_group14.

%%%----------------------------------------------------------------
generate_key(Algorithm, Args) ->
    {Public,Private} = crypto:generate_key(Algorithm, Args),
    {crypto:bytes_to_integer(Public), crypto:bytes_to_integer(Private)}.

      
compute_key(Algorithm, OthersPublic, MyPrivate, Args) ->
    Shared = crypto:compute_key(Algorithm, OthersPublic, MyPrivate, Args),
    crypto:bytes_to_integer(Shared).


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


%% default_algorithms(kex) -> % Example of how to disable an algorithm
%%     supported_algorithms(kex, ['ecdh-sha2-nistp521']);

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


