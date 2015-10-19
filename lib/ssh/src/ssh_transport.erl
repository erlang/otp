%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2014. All Rights Reserved.
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
-export([next_seqnum/1, decrypt_first_block/2, decrypt_blocks/3,
	 supported_algorithms/0, supported_algorithms/1,
	 default_algorithms/0, default_algorithms/1,
	 is_valid_mac/3,
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
	 unpack/3, decompress/2, ssh_packet/2, pack/2, msg_data/1,
	 sign/3, verify/4]).

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

%% default_algorithms(kex) -> % Example of how to disable an algorithm
%%     supported_algorithms(kex, ['ecdh-sha2-nistp521']);
default_algorithms(Alg) ->
    supported_algorithms(Alg).


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
	[{'aes256-ctr', [{ciphers,{aes_ctr,256}}]},
	 {'aes192-ctr', [{ciphers,{aes_ctr,192}}]},
	 {'aes128-ctr', [{ciphers,{aes_ctr,128}}]},
	 {'aes128-cbc', [{ciphers,aes_cbc128}]},
	 {'3des-cbc',   [{ciphers,des3_cbc}]}
	]
       ));
supported_algorithms(mac) ->
    same(
      select_crypto_supported(
	[{'hmac-sha2-256', [{hashs,sha256}]},
	 {'hmac-sha2-512', [{hashs,sha512}]},
	 {'hmac-sha1',     [{hashs,sha}]}
	]
       ));
supported_algorithms(compression) ->
    same(['none',
 	  'zlib@openssh.com',
	  'zlib'
	 ]).

%% Dialyzer complains when not called...supported_algorithms(Key, [{client2server,BL1},{server2client,BL2}]) ->
%% Dialyzer complains when not called...    [{client2server,As1},{server2client,As2}] = supported_algorithms(Key),
%% Dialyzer complains when not called...    [{client2server,As1--BL1},{server2client,As2--BL2}];
%% Dialyzer complains when not called...supported_algorithms(Key, BlackList) ->
%% Dialyzer complains when not called...    supported_algorithms(Key) -- BlackList.

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
		  ({Tag,{Name=aes_ctr,Len}}) when is_integer(Len) ->
		       crypto_name_supported(Tag,Name,Supported) andalso
			   ctr_len_supported(Name,Len)
	       end, Conditions).

crypto_name_supported(Tag, CryptoName, Supported) ->
    lists:member(CryptoName, proplists:get_value(Tag,Supported,[])).

ctr_len_supported(Name, Len) ->
    try
	crypto:stream_encrypt(crypto:stream_init(Name, <<0:Len>>, <<0:128>>), <<"">>)
    of
	{_,X} -> is_binary(X)
    catch
	_:_ -> false
    end.
	    

same(Algs) ->  [{client2server,Algs}, {server2client,Algs}].


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

decrypt_first_block(Bin, #ssh{decrypt_block_size = BlockSize} = Ssh0) ->
    <<EncBlock:BlockSize/binary, EncData/binary>> = Bin,
    {Ssh, <<?UINT32(PacketLen), _/binary>> = DecData} =
	decrypt(Ssh0, EncBlock),
    {Ssh, PacketLen, DecData, EncData}.
    
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
	    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
				      description = "Selection of key exchange"
				      " algorithm failed", 
				      language = ""})
    end;

handle_kexinit_msg(#ssh_msg_kexinit{} = CounterPart, #ssh_msg_kexinit{} = Own,
			    #ssh{role = server} = Ssh) ->
    {ok, Algoritms} = select_algorithm(server, CounterPart, Own),
    case  verify_algorithm(Algoritms) of
	true ->
	    {ok, Ssh#ssh{algorithms = Algoritms}};
	_ ->
	    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
				      description = "Selection of key exchange"
				      " algorithm failed",
				      language = ""})
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
	    throw({{error,bad_e_from_peer},
		   #ssh_msg_disconnect{
		      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		      description = "Key exchange failed, 'e' out of bounds",
		      language = ""}
		   })
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
		    throw({Error,
			   #ssh_msg_disconnect{
			      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			      description = "Key exchange failed",
			      language = "en"}
			  })
	    end;

	true ->
	    throw({{error,bad_f_from_peer},
		   #ssh_msg_disconnect{
		      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		      description = "Key exchange failed, 'f' out of bounds",
		      language = ""}
		   })
    end.


%%%----------------------------------------------------------------
%%%
%%% diffie-hellman-group-exchange-sha1
%%% 
handle_kex_dh_gex_request(#ssh_msg_kex_dh_gex_request{min = Min,
						      n   = NBits,
						      max = Max}, 
			  Ssh0=#ssh{opts=Opts}) when Min=<NBits, NBits=<Max ->
    %% server
    {G, P} = dh_gex_group(Min, NBits, Max, proplists:get_value(dh_gex_groups,Opts)),
    {Public, Private} = generate_key(dh, [P,G]),
    {SshPacket, Ssh} = 
	ssh_packet(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0),
    {ok, SshPacket, 
     Ssh#ssh{keyex_key = {{Private, Public}, {G, P}},
	     keyex_info = {Min, Max, NBits}
	    }};
handle_kex_dh_gex_request(_, _) ->
  throw({{error,bad_ssh_msg_kex_dh_gex_request},
	 #ssh_msg_disconnect{
	    code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
	    description = "Key exchange failed, bad values in ssh_msg_kex_dh_gex_request",
	    language = ""}
	}).

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
		    throw({{error,bad_K},
			   #ssh_msg_disconnect{
			      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			      description = "Key exchange failed, 'K' out of bounds",
			      language = ""}
			  })
	    end;
	true ->
	    throw({{error,bad_e_from_peer},
		   #ssh_msg_disconnect{
		      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		      description = "Key exchange failed, 'e' out of bounds",
		      language = ""}
		  })
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
			    throw(#ssh_msg_disconnect{
				     code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
				     description = "Key exchange failed",
				     language = ""}
				 )
		    end;

		true ->
		    throw({{error,bad_K},
			   #ssh_msg_disconnect{
			      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			      description = "Key exchange failed, 'K' out of bounds",
			      language = ""}
			  })
	    end;
	true ->
	    throw({{error,bad_f_from_peer},
		   #ssh_msg_disconnect{
		      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		      description = "Key exchange failed, 'f' out of bounds",
		      language = ""}
		  })
    end.	       

%%%----------------------------------------------------------------
%%%
%%% diffie-hellman-ecdh-sha2-*
%%% 
handle_kex_ecdh_init(#ssh_msg_kex_ecdh_init{q_c = PeerPublic},
		     Ssh0 = #ssh{algorithms = #alg{kex=Kex}}) ->
    %% at server
    Curve = ecdh_curve(Kex),
    case ecdh_validate_public_key(PeerPublic, Curve) of
	true ->
            {MyPublic, MyPrivate} = generate_key(ecdh, Curve),
	    K = compute_key(ecdh, PeerPublic, MyPrivate, Curve),
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
				     session_id = sid(Ssh1, H)}};

	false ->
	    throw({{error,invalid_peer_public_key},
		   #ssh_msg_disconnect{
		      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		      description = "Peer ECDH public key is invalid",
		      language = ""}
		  })
    end.

handle_kex_ecdh_reply(#ssh_msg_kex_ecdh_reply{public_host_key = PeerPubHostKey,
					      q_s = PeerPublic,
					      h_sig = H_SIG},
		      #ssh{keyex_key = {{MyPublic,MyPrivate}, Curve}} = Ssh0
		     ) ->
    %% at client
    case ecdh_validate_public_key(PeerPublic, Curve) of
	true ->
	    K = compute_key(ecdh, PeerPublic, MyPrivate, Curve),
	    H = kex_h(Ssh0, Curve, PeerPubHostKey, MyPublic, PeerPublic, K), 
	    case verify_host_key(Ssh0, PeerPubHostKey, H, H_SIG) of
		ok ->
		    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
		    {ok, SshPacket, Ssh#ssh{shared_secret  = K,
					    exchanged_hash = H,
					    session_id = sid(Ssh, H)}};
		Error ->
		    throw({Error,
			   #ssh_msg_disconnect{
			      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			      description = "Key exchange failed",
			      language = ""}
			  })
	    end;

	false ->
	    throw({{error,invalid_peer_public_key},
		   #ssh_msg_disconnect{
		      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
		      description = "Peer ECDH public key is invalid",
		      language = ""}
		  })
    end.


%%%----------------------------------------------------------------
%%%
%%% Standards for Efficient Cryptography Group, "Elliptic Curve Cryptography", SEC 1
%%% Section 3.2.2.1
%%%

ecdh_validate_public_key(Key, Curve) ->
    case key_size(Curve) of
	undefined ->
	    false;

	Sz ->
	    case dec_key(Key, Sz) of
		{ok,Q} ->
		    case crypto:ec_curve(Curve) of
			{{prime_field,P}, {A, B, _Seed},
			 _P0Bin, _OrderBin, _CoFactorBin} ->
			    on_curve(Q, bin2int(A), bin2int(B), bin2int(P))
		    end;

		{error,compressed_not_implemented} -> % Be a bit generous...
		    true;

		_Error -> 
		    false
	    end
    end.


on_curve({X,Y}, A, B, P) when 0 =< X,X =< (P-1),
			      0 =< Y,Y =< (P-1) ->
    %% Section 3.2.2.1, point 2
    (Y*Y) rem P == (X*X*X + A*X + B) rem P;
on_curve(_, _, _, _) ->
    false.


bin2int(B) ->
    Sz = erlang:bit_size(B),
    <<I:Sz/big-unsigned-integer>> = B,
    I.

key_size(secp256r1) -> 256;
key_size(secp384r1) -> 384;
key_size(secp521r1) -> 528; % Round 521 up to closest 8-bits.
key_size(_) -> undefined.


dec_key(Key, NBits) ->
    Size = 8 + 2*NBits,
    case <<Key:Size>> of
	<<4:8, X:NBits, Y:NBits>> -> {ok,{X,Y}};
	<<4:8, _/binary>> -> {error,bad_format};
	_ -> {error,compressed_not_implemented}
    end.

%%%----------------------------------------------------------------
handle_new_keys(#ssh_msg_newkeys{}, Ssh0) ->
    try install_alg(Ssh0) of
	#ssh{} = Ssh ->
	    {ok, Ssh}
    catch 
	_C:_Error -> %% TODO: Throw earlier ....
	    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				      description = "Install alg failed", 
				      language = "en"})
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
    {Encrypt, Decrypt} = select_encrypt_decrypt(Role, Client, Server),
    {SendMac, RecvMac} = select_send_recv_mac(Role, Client, Server),
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
    Decomprssion = 
	select(Client#ssh_msg_kexinit.compression_algorithms_server_to_client,
	       Server#ssh_msg_kexinit.compression_algorithms_server_to_client),
    {Compression, Decomprssion};
select_compression_decompression(server, Client, Server) ->
    Decomprssion = 
	select(Client#ssh_msg_kexinit.compression_algorithms_client_to_server,
	       Server#ssh_msg_kexinit.compression_algorithms_client_to_server),
    Compression = 
	select(Client#ssh_msg_kexinit.compression_algorithms_server_to_client,
	       Server#ssh_msg_kexinit.compression_algorithms_server_to_client),
    {Compression, Decomprssion}.

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
    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
			      description = Err,
			      language = ""}).


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

pack(Data0, #ssh{encrypt_block_size = BlockSize, 
		 send_sequence = SeqNum, send_mac = MacAlg,
		 send_mac_key = MacKey,
		 random_length_padding = RandomLengthPadding} 
     = Ssh0) when is_binary(Data0) ->
    {Ssh1, Data} = compress(Ssh0, Data0),
    PL = (BlockSize - ((4 + 1 + size(Data)) rem BlockSize)) rem BlockSize,
    MinPaddingLen = if PL <  4 -> PL + BlockSize;
		       true -> PL
		    end,
    PadBlockSize =  max(BlockSize,4),
    MaxExtraBlocks = (max(RandomLengthPadding,MinPaddingLen) - MinPaddingLen) div PadBlockSize,
    ExtraPaddingLen = try crypto:rand_uniform(0,MaxExtraBlocks)*PadBlockSize
		      catch _:_ -> 0
		      end,
    PaddingLen = MinPaddingLen + ExtraPaddingLen,
    Padding = ssh_bits:random(PaddingLen),
    PacketLen = 1 + PaddingLen + size(Data),
    PacketData = <<?UINT32(PacketLen),?BYTE(PaddingLen), 
		  Data/binary, Padding/binary>>,
    {Ssh2, EncPacket} = encrypt(Ssh1, PacketData),
    MAC = mac(MacAlg, MacKey, SeqNum, PacketData),
    Packet = [EncPacket, MAC],
    Ssh = Ssh2#ssh{send_sequence = (SeqNum+1) band 16#ffffffff},
    {Packet, Ssh}.

unpack(EncodedSoFar, ReminingLenght, #ssh{recv_mac_size = MacSize} = Ssh0) ->
    SshLength = ReminingLenght - MacSize,
    {NoMac, Mac, Rest} = case MacSize of
			     0 ->
				 <<NoMac0:SshLength/binary, 
				  Rest0/binary>> = EncodedSoFar,
				 {NoMac0, <<>>, Rest0};
			     _ ->
				 <<NoMac0:SshLength/binary, 
				  Mac0:MacSize/binary,
				  Rest0/binary>> = EncodedSoFar,
				 {NoMac0, Mac0, Rest0}
			 end,
    {Ssh1, DecData, <<>>} = 
	case SshLength of
	    0 ->
		{Ssh0, <<>>, <<>>};
	    _ ->
		decrypt_blocks(NoMac, SshLength, Ssh0)
	end,
    {Ssh1, DecData, Rest, Mac}.

msg_data(PacketData) ->
    <<Len:32, PaddingLen:8, _/binary>> = PacketData,
    DataLen = Len - PaddingLen - 1,
    <<_:32, _:8, Data:DataLen/binary, 
     _:PaddingLen/binary>> = PacketData,
    Data.

sign(SigData, Hash,  #'DSAPrivateKey'{} = Key) ->
    DerSignature = public_key:sign(SigData, Hash, Key),
    #'Dss-Sig-Value'{r = R, s = S} = public_key:der_decode('Dss-Sig-Value', DerSignature),
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>>;
sign(SigData, Hash, Key = #'ECPrivateKey'{}) ->
    DerEncodedSign =  public_key:sign(SigData, Hash, Key),
    #'ECDSA-Sig-Value'{r=R, s=S} = public_key:der_decode('ECDSA-Sig-Value', DerEncodedSign),
    ssh_bits:encode([R,S], [mpint,mpint]);
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

decrypt(#ssh{decrypt = none} = Ssh, Data) ->
    {Ssh, Data};
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
    case SSH#ssh.role of
	client ->
	    KeySize =mac_key_size(SSH#ssh.send_mac),
	    Key = hash(SSH, "E", KeySize),
	    {ok, SSH#ssh { send_mac_key = Key }};
	server ->
	    KeySize = mac_key_size(SSH#ssh.send_mac),
	    Key = hash(SSH, "F", KeySize),
	    {ok, SSH#ssh { send_mac_key = Key }}
    end.

send_mac_final(SSH) ->
    {ok, SSH#ssh {  send_mac = none, send_mac_key = undefined }}.

recv_mac_init(SSH) ->
    case SSH#ssh.role of
	client ->
	    Key = hash(SSH, "F", mac_key_size(SSH#ssh.recv_mac)),
	    {ok, SSH#ssh { recv_mac_key = Key }};
	server ->
	    Key = hash(SSH, "E", mac_key_size(SSH#ssh.recv_mac)),
	    {ok, SSH#ssh { recv_mac_key = Key }}
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
    L = ssh_bits:encode([SSH#ssh.c_version, SSH#ssh.s_version,
			 SSH#ssh.c_keyinit, SSH#ssh.s_keyinit,
			 KeyBin, E,F,K],
			[string,string,binary,binary,binary,
			 mpint,mpint,mpint]),
    crypto:hash(sha((SSH#ssh.algorithms)#alg.kex), L).
%%  crypto:hash(sha,L).

kex_h(SSH, Curve, Key, Q_c, Q_s, K) ->
    KeyBin = public_key:ssh_encode(Key, ssh2_pubkey),
    L = ssh_bits:encode([SSH#ssh.c_version, SSH#ssh.s_version,
			 SSH#ssh.c_keyinit, SSH#ssh.s_keyinit,
			 KeyBin, Q_c, Q_s, K],
			[string,string,binary,binary,binary,
			 mpint,mpint,mpint]),
    crypto:hash(sha(Curve), L).

kex_h(SSH, Key, Min, NBits, Max, Prime, Gen, E, F, K) ->
    L = if Min==-1; Max==-1 ->
		KeyBin = public_key:ssh_encode(Key, ssh2_pubkey),
		Ts = [string,string,binary,binary,binary,
		      uint32,
		      mpint,mpint,mpint,mpint,mpint],
		ssh_bits:encode([SSH#ssh.c_version,SSH#ssh.s_version,
				 SSH#ssh.c_keyinit,SSH#ssh.s_keyinit,
				 KeyBin, NBits, Prime, Gen, E,F,K],
				Ts);
	   true ->
		KeyBin = public_key:ssh_encode(Key, ssh2_pubkey),
		Ts = [string,string,binary,binary,binary,
		      uint32,uint32,uint32,
		      mpint,mpint,mpint,mpint,mpint],
		ssh_bits:encode([SSH#ssh.c_version,SSH#ssh.s_version,
				 SSH#ssh.c_keyinit,SSH#ssh.s_keyinit,
				 KeyBin, Min, NBits, Max,
				 Prime, Gen, E,F,K], Ts)
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
mac_digest_size(none) -> 0.

peer_name({Host, _}) ->
    Host.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Diffie-Hellman utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dh_group('diffie-hellman-group1-sha1') ->  element(2, ?dh_group1);
dh_group('diffie-hellman-group14-sha1') -> element(2, ?dh_group14).

dh_gex_default_groups() -> ?dh_default_groups.


dh_gex_group(Min, N, Max, undefined) ->
    dh_gex_group(Min, N, Max, dh_gex_default_groups());
dh_gex_group(Min, N, Max, Groups) ->
    %% First try to find an exact match. If not an exact match, select the largest possible.
    {_Size,Group} =
	lists:foldl(
	  fun(_, {I,G}) when I==N ->
		  %% If we have an exact match already: use that one
		  {I,G};
	     ({I,G}, _) when I==N ->
		  %% If we now found an exact match: use that very one
		  {I,G};
	     ({I,G}, {Imax,_Gmax}) when Min=<I,I=<Max, % a) {I,G} fullfills the requirements
				        I>Imax ->      % b) {I,G} is larger than current max
		  %% A group within the limits and better than the one we have
		  {I,G};
	     (_, IGmax) ->
		  %% Keep the one we have
		  IGmax
	  end, {-1,undefined}, Groups),

    case Group of
	undefined ->
	    throw(#ssh_msg_disconnect{
		     code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
		     description = "No possible diffie-hellman-group-exchange group found", 
		     language = ""});
	_ ->
	    Group
    end.


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


