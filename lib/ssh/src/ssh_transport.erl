%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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

%%% Description: SSH transport protocol

-module(ssh_transport).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/inet.hrl").

-include("ssh_transport.hrl").
-include("ssh.hrl").

-export([versions/2, hello_version_msg/1]).
-export([next_seqnum/1, decrypt_first_block/2, decrypt_blocks/3,
	 is_valid_mac/3,
	 handle_hello_version/1,
	 key_exchange_init_msg/1,
	 key_init/3, new_keys_message/1,
	 handle_kexinit_msg/3, handle_kexdh_init/2,
	 handle_kex_dh_gex_group/2, handle_kex_dh_gex_reply/2,
	 handle_new_keys/2, handle_kex_dh_gex_request/2,
	 handle_kexdh_reply/2, 
	 unpack/3, decompress/2, ssh_packet/2, pack/2, msg_data/1,
	 sign/3, verify/4]).

versions(client, Options)->
    Vsn = proplists:get_value(vsn, Options, ?DEFAULT_CLIENT_VERSION),
    Version = format_version(Vsn),
    {Vsn, Version};
versions(server, Options) ->
    Vsn = proplists:get_value(vsn, Options, ?DEFAULT_SERVER_VERSION),
    Version = format_version(Vsn),
    {Vsn, Version}.

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

format_version({Major,Minor}) ->
    "SSH-" ++ integer_to_list(Major) ++ "." ++ 
	integer_to_list(Minor) ++ "-Erlang".

handle_hello_version(Version) ->
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
    end.

key_exchange_init_msg(Ssh0) ->
    Msg = kex_init(Ssh0),
    {SshPacket, Ssh} = ssh_packet(Msg, Ssh0),
    {Msg, SshPacket, Ssh}.

kex_init(#ssh{role = Role, opts = Opts, available_host_keys = HostKeyAlgs}) ->
    Random = ssh_bits:random(16),
    Compression = case proplists:get_value(compression, Opts, none) of
		      openssh_zlib -> ["zlib@openssh.com", "none"];
		      zlib -> ["zlib", "none"];
		      none -> ["none", "zlib"]
		  end,
    kexinit_messsage(Role, Random, Compression, HostKeyAlgs).

key_init(client, Ssh, Value) ->
    Ssh#ssh{c_keyinit = Value};
key_init(server, Ssh, Value) ->
    Ssh#ssh{s_keyinit = Value}.

kexinit_messsage(client, Random, Compression, HostKeyAlgs) ->
    #ssh_msg_kexinit{ 
		  cookie = Random,
		  kex_algorithms = ["diffie-hellman-group1-sha1"],
		  server_host_key_algorithms = HostKeyAlgs,
		  encryption_algorithms_client_to_server = ["aes128-cbc","3des-cbc"],
		  encryption_algorithms_server_to_client = ["aes128-cbc","3des-cbc"],
		  mac_algorithms_client_to_server = ["hmac-sha1"],
		  mac_algorithms_server_to_client = ["hmac-sha1"],
		  compression_algorithms_client_to_server = Compression,
		  compression_algorithms_server_to_client = Compression,
		  languages_client_to_server = [],
		  languages_server_to_client = []
		 };

kexinit_messsage(server, Random, Compression, HostKeyAlgs) ->
    #ssh_msg_kexinit{
		  cookie = Random,
		  kex_algorithms = ["diffie-hellman-group1-sha1"],
		  server_host_key_algorithms = HostKeyAlgs,
		  encryption_algorithms_client_to_server = ["aes128-cbc","3des-cbc"],
		  encryption_algorithms_server_to_client = ["aes128-cbc","3des-cbc"],
		  mac_algorithms_client_to_server = ["hmac-sha1"],
		  mac_algorithms_server_to_client = ["hmac-sha1"],
		  compression_algorithms_client_to_server = Compression,
		  compression_algorithms_server_to_client = Compression,
		  languages_client_to_server = [],
		  languages_server_to_client = []
		 }.

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
	    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				      description = "Selection of key exchange"
				      " algorithm failed", 
				      language = "en"})
    end;

handle_kexinit_msg(#ssh_msg_kexinit{} = CounterPart, #ssh_msg_kexinit{} = Own,
			    #ssh{role = server} = Ssh) ->
    {ok, Algoritms} = select_algorithm(server, CounterPart, Own),
    {ok, Ssh#ssh{algorithms = Algoritms}}.


%% TODO: diffie-hellman-group14-sha1 should also be supported.
%% Maybe check more things ...
verify_algorithm(#alg{kex = 'diffie-hellman-group1-sha1'}) ->
    true;
verify_algorithm(#alg{kex = 'diffie-hellman-group-exchange-sha1'}) ->
    true;
verify_algorithm(_) ->
    false.

key_exchange_first_msg('diffie-hellman-group1-sha1', Ssh0) ->
    {G, P} = dh_group1(),
    {Private, Public} = dh_gen_key(G, P, 1024),
    {SshPacket, Ssh1} = ssh_packet(#ssh_msg_kexdh_init{e = Public}, Ssh0),
    {ok, SshPacket, 
     Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}}}};

key_exchange_first_msg('diffie-hellman-group-exchange-sha1', Ssh0) ->
    Min = ?DEFAULT_DH_GROUP_MIN,
    NBits = ?DEFAULT_DH_GROUP_NBITS,
    Max = ?DEFAULT_DH_GROUP_MAX,
    {SshPacket, Ssh1} = 
	ssh_packet(#ssh_msg_kex_dh_gex_request{min = Min, 
					       n = NBits, max = Max}, 
		   Ssh0),
    {ok, SshPacket, 
     Ssh1#ssh{keyex_info = {Min, Max, NBits}}}. 


handle_kexdh_init(#ssh_msg_kexdh_init{e = E}, Ssh0) ->
    {G, P} = dh_group1(),
    {Private, Public} = dh_gen_key(G, P, 1024),
    K = ssh_math:ipow(E, Private, P),
    Key = get_host_key(Ssh0),
    H = kex_h(Ssh0, Key, E, Public, K),
    H_SIG = sign_host_key(Ssh0, Key, H),
    {SshPacket, Ssh1} = ssh_packet(#ssh_msg_kexdh_reply{public_host_key = Key,
							f = Public,
							h_sig = H_SIG
						       }, Ssh0),

    {ok, SshPacket, Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}},
			     shared_secret = K,
			     exchanged_hash = H,
			     session_id = sid(Ssh1, H)}}.

handle_kex_dh_gex_group(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0) ->
    {Private, Public} = dh_gen_key(G,P,1024),
    {SshPacket, Ssh1} = 
	ssh_packet(#ssh_msg_kex_dh_gex_init{e = Public}, Ssh0),
    {ok, SshPacket, 
     Ssh1#ssh{keyex_key = {{Private, Public}, {G, P}}}}.

handle_new_keys(#ssh_msg_newkeys{}, Ssh0) ->
    try install_alg(Ssh0) of
	#ssh{} = Ssh ->
	    {ok, Ssh}
    catch 
	error:_Error -> %% TODO: Throw earlier ....
	    throw(#ssh_msg_disconnect{code = ?SSH_DISCONNECT_PROTOCOL_ERROR,
				      description = "Install alg failed", 
				      language = "en"})
    end. 


%% %% Select algorithms
handle_kexdh_reply(#ssh_msg_kexdh_reply{public_host_key = HostKey, f = F,
					h_sig = H_SIG}, 
		   #ssh{keyex_key = {{Private, Public}, {_G, P}}} = Ssh0) ->
    K = ssh_math:ipow(F, Private, P),
    H = kex_h(Ssh0, HostKey, Public, F, K),

    case verify_host_key(Ssh0, HostKey, H, H_SIG) of
	ok ->
	    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
	    {ok, SshPacket, Ssh#ssh{shared_secret  = K,
				    exchanged_hash = H,
				    session_id = sid(Ssh, H)}};
	Error ->
	    Disconnect = #ssh_msg_disconnect{
	      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
	      description = "Key exchange failed",
	      language = "en"},
	    throw({Error, Disconnect})
    end.

handle_kex_dh_gex_request(#ssh_msg_kex_dh_gex_request{min = _Min,
						      n   = _NBits,
						      max = _Max}, Ssh0) ->
    {G,P} = dh_group1(), %% TODO real imp this seems to be a hack?!
    {Private, Public} = dh_gen_key(G, P, 1024),
    {SshPacket, Ssh} = 
	ssh_packet(#ssh_msg_kex_dh_gex_group{p = P, g = G}, Ssh0),
    {ok, SshPacket, 
     Ssh#ssh{keyex_key = {{Private, Public}, {G, P}}}}.

handle_kex_dh_gex_reply(#ssh_msg_kex_dh_gex_reply{public_host_key = HostKey, 
						  f = F,
						  h_sig = H_SIG},
			#ssh{keyex_key = {{Private, Public}, {G, P}},
			     keyex_info = {Min, Max, NBits}} = 
		 	Ssh0) ->
    K = ssh_math:ipow(F, Private, P),
    H = kex_h(Ssh0, HostKey, Min, NBits, Max, P, G, Public, F, K),

    case verify_host_key(Ssh0, HostKey, H, H_SIG) of
	ok ->
	    {SshPacket, Ssh} = ssh_packet(#ssh_msg_newkeys{}, Ssh0),
	    {ok, SshPacket, Ssh#ssh{shared_secret  = K,
			  exchanged_hash = H,
			  session_id = sid(Ssh, H)}};
	_Error ->
	    Disconnect = #ssh_msg_disconnect{
	      code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
	      description = "Key exchange failed",
	      language = "en"},
	    throw(Disconnect)
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
	{ok, #'RSAPrivateKey'{} = Key} ->
	    Key;
	{ok, #'DSAPrivateKey'{} = Key} ->
	    Key;
	Result ->
	    exit({error, {Result, unsupported_key_type}})
    end.

sign_host_key(_Ssh, #'RSAPrivateKey'{} = Private, H) ->
    Hash = sha, %% Option ?!
    _Signature = sign(H, Hash, Private);
sign_host_key(_Ssh, #'DSAPrivateKey'{} = Private, H) ->
    Hash = sha, %% Option ?!
    _RawSignature = sign(H, Hash, Private).

verify_host_key(SSH, PublicKey, Digest, Signature) ->
    case verify(Digest, sha, Signature, PublicKey) of
	false ->
	    {error, bad_signature};
	true ->
	    known_host_key(SSH, PublicKey, public_algo(PublicKey))
    end.

public_algo(#'RSAPublicKey'{}) ->
    'ssh-rsa';
public_algo({_, #'Dss-Parms'{}}) ->
    'ssh-dss'.

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

select_all(CL, SL) ->
    A = CL -- SL,  %% algortihms only used by client
    %% algorithms used by client and server (client pref)
    lists:map(fun(ALG) -> list_to_atom(ALG) end, (CL -- A)).

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
		 send_mac_key = MacKey} 
     = Ssh0) when is_binary(Data0) ->
    {Ssh1, Data} = compress(Ssh0, Data0),
    PL = (BlockSize - ((4 + 1 + size(Data)) rem BlockSize)) rem BlockSize,
    PaddingLen = if PL <  4 -> PL + BlockSize;
		    true -> PL
		 end,
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
sign(SigData, Hash, Key) ->
    public_key:sign(SigData, Hash, Key).

verify(PlainText, Hash, Sig, {_,  #'Dss-Parms'{}} = Key) ->
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>> = Sig,
    Signature = public_key:der_encode('Dss-Sig-Value', #'Dss-Sig-Value'{r = R, s = S}),
    public_key:verify(PlainText, Hash, Signature, Key);
verify(PlainText, Hash, Sig, Key) ->
    public_key:verify(PlainText, Hash, Sig, Key).

%% public key algorithms
%%
%%   ssh-dss              REQUIRED     sign    Raw DSS Key
%%   ssh-rsa              RECOMMENDED  sign    Raw RSA Key
%%   x509v3-sign-rsa      OPTIONAL     sign    X.509 certificates (RSA key)
%%   x509v3-sign-dss      OPTIONAL     sign    X.509 certificates (DSS key)
%%   spki-sign-rsa        OPTIONAL     sign    SPKI certificates (RSA key)
%%   spki-sign-dss        OPTIONAL     sign    SPKI certificates (DSS key)
%%   pgp-sign-rsa         OPTIONAL     sign    OpenPGP certificates (RSA key)
%%   pgp-sign-dss         OPTIONAL     sign    OpenPGP certificates (DSS key)
%%

%% key exchange
%%
%%     diffie-hellman-group1-sha1       REQUIRED
%%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encryption
%%
%% chiphers
%%
%%       3des-cbc         REQUIRED          
%%       three-key 3DES in CBC mode
%%       blowfish-cbc     OPTIONAL          Blowfish in CBC mode
%%       twofish256-cbc   OPTIONAL          Twofish in CBC mode,
%%                                          with 256-bit key
%%       twofish-cbc      OPTIONAL          alias for "twofish256-cbc" (this
%%                                          is being retained for
%%                                          historical reasons)
%%       twofish192-cbc   OPTIONAL          Twofish with 192-bit key
%%       twofish128-cbc   OPTIONAL          Twofish with 128-bit key
%%       aes256-cbc       OPTIONAL          AES in CBC mode,
%%                                          with 256-bit key
%%       aes192-cbc       OPTIONAL          AES with 192-bit key
%%       aes128-cbc       RECOMMENDED       AES with 128-bit key
%%       serpent256-cbc   OPTIONAL          Serpent in CBC mode, with
%%                                          256-bit key
%%       serpent192-cbc   OPTIONAL          Serpent with 192-bit key
%%       serpent128-cbc   OPTIONAL          Serpent with 128-bit key
%%       arcfour          OPTIONAL          the ARCFOUR stream cipher
%%       idea-cbc         OPTIONAL          IDEA in CBC mode
%%       cast128-cbc      OPTIONAL          CAST-128 in CBC mode
%%       none             OPTIONAL          no encryption; NOT RECOMMENDED
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
                 encrypt_ctx = IV}}.

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
    {Ssh#ssh{encrypt_ctx = IV}, Enc}.
  

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
		 decrypt_block_size = 16}}.

  
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
    {Ssh#ssh{decrypt_ctx = IV}, Dec}.

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
%% MAC calculation
%%
%%     hmac-sha1    REQUIRED        HMAC-SHA1 (digest length = key
%%                                  length = 20)
%%     hmac-sha1-96 RECOMMENDED     first 96 bits of HMAC-SHA1 (digest
%%                                  length = 12, key length = 20)
%%     hmac-md5     OPTIONAL        HMAC-MD5 (digest length = key
%%                                  length = 16)
%%     hmac-md5-96  OPTIONAL        first 96 bits of HMAC-MD5 (digest
%%                                  length = 12, key length = 16)
%%     none         OPTIONAL        no MAC; NOT RECOMMENDED
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
    crypto:hmac(md5, Key, [<<?UINT32(SeqNum)>>, Data], mac_digest_size('hmac-md5-96')).

%% return N hash bytes (HASH)
hash(SSH, Char, Bits) ->
    HASH =
	case SSH#ssh.kex of
	    'diffie-hellman-group1-sha1' ->
		fun(Data) -> crypto:hash(sha, Data) end;
	    'diffie-hellman-group-exchange-sha1' ->
		fun(Data) -> crypto:hash(sha, Data) end;
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
    L = ssh_bits:encode([SSH#ssh.c_version, SSH#ssh.s_version,
			 SSH#ssh.c_keyinit, SSH#ssh.s_keyinit,
			 ssh_message:encode_host_key(Key), E,F,K],
			[string,string,binary,binary,binary,
			 mpint,mpint,mpint]),
    crypto:hash(sha,L).
 

kex_h(SSH, Key, Min, NBits, Max, Prime, Gen, E, F, K) ->
    L = if Min==-1; Max==-1 ->
		Ts = [string,string,binary,binary,binary,
		      uint32,
		      mpint,mpint,mpint,mpint,mpint],
		ssh_bits:encode([SSH#ssh.c_version,SSH#ssh.s_version,
				 SSH#ssh.c_keyinit,SSH#ssh.s_keyinit,
				 ssh_message:encode_host_key(Key), NBits, Prime, Gen, E,F,K],
				Ts);
	   true ->
		Ts = [string,string,binary,binary,binary,
		      uint32,uint32,uint32,
		      mpint,mpint,mpint,mpint,mpint],
		ssh_bits:encode([SSH#ssh.c_version,SSH#ssh.s_version,
				 SSH#ssh.c_keyinit,SSH#ssh.s_keyinit,
				 ssh_message:encode_host_key(Key), Min, NBits, Max,
				 Prime, Gen, E,F,K], Ts)
	end,
    crypto:hash(sha,L).
  
mac_key_size('hmac-sha1')    -> 20*8;
mac_key_size('hmac-sha1-96') -> 20*8;
mac_key_size('hmac-md5')     -> 16*8;
mac_key_size('hmac-md5-96')  -> 16*8;
mac_key_size(none) -> 0.

mac_digest_size('hmac-sha1')    -> 20;
mac_digest_size('hmac-sha1-96') -> 12;
mac_digest_size('hmac-md5')    -> 20;
mac_digest_size('hmac-md5-96') -> 12;
mac_digest_size(none) -> 0.

peer_name({Host, _}) ->
    Host.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Diffie-Hellman utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dh_group1() ->
    {2, 16#FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF}.

dh_gen_key(G, P, _) ->
    {Public, Private} = crypto:generate_key(dh, [P, G]),
    {crypto:bytes_to_integer(Private), crypto:bytes_to_integer(Public)}.

trim_tail(Str) ->
    lists:reverse(trim_head(lists:reverse(Str))).

trim_head([$\s|Cs]) -> trim_head(Cs);
trim_head([$\t|Cs]) -> trim_head(Cs);
trim_head([$\n|Cs]) -> trim_head(Cs);
trim_head([$\r|Cs]) -> trim_head(Cs);
trim_head(Cs) -> Cs.


