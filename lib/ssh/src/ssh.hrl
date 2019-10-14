%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

%%
%% SSH definitions
%%

-ifndef(SSH_HRL).
-define(SSH_HRL, 1).

-define(SSH_DEFAULT_PORT, 22).
-define(SSH_MAX_PACKET_SIZE, (256*1024)).
-define(REKEY_DATA_TIMOUT, 60000).
-define(DEFAULT_PROFILE, default).

-define(DEFAULT_TRANSPORT,  {tcp, gen_tcp, tcp_closed} ).

-define(DEFAULT_SHELL, {shell, start, []} ).

-define(MAX_RND_PADDING_LEN, 15).

-define(SUPPORTED_AUTH_METHODS, "publickey,keyboard-interactive,password").

-define(FALSE, 0).
-define(TRUE,  1).
%% basic binary constructors
-define(BOOLEAN(X),  (X):8/unsigned-big-integer).
-define(BYTE(X),     (X):8/unsigned-big-integer).
-define(UINT16(X),   (X):16/unsigned-big-integer).
-define(UINT32(X),   (X):32/unsigned-big-integer).
-define(UINT64(X),   (X):64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((size(X))), (X)/binary).

-define(DEC_BIN(X,Len),   ?UINT32(Len), X:Len/binary ).
-define(DEC_MPINT(I,Len), ?UINT32(Len), I:Len/big-signed-integer-unit:8 ).

%% building macros
-define(boolean(X),
	case X of
	    true -> <<?BOOLEAN(1)>>;
	    false -> (<<?BOOLEAN(0)>>)
	end).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
-define(uint64(X), << ?UINT64(X) >> ).
-define(string_utf8(X), << ?STRING(unicode:characters_to_binary(X)) >> ).
-define(string(X), ?string_utf8(X)).
-define(binary(X), << ?STRING(X) >>).

-define('2bin'(X), (if is_binary(X) -> X;
		       is_list(X) -> list_to_binary(X);
		       X==undefined -> <<>>
		    end) ).

%% encoding macros
-define('E...'(X),    ?'2bin'(X)/binary ).
-define(Eboolean(X),  ?BOOLEAN(case X of
				   true -> ?TRUE;
				   false -> ?FALSE
			       end) ).
-define(Ebyte(X),        ?BYTE(X) ).
-define(Euint32(X),      ?UINT32(X) ).
-define(Estring(X),      ?STRING(?'2bin'(X)) ).
-define(Estring_utf8(X), ?string_utf8(X)/binary ).
-define(Ename_list(X),   ?STRING(ssh_bits:name_list(X)) ).
-define(Empint(X),       (ssh_bits:mpint(X))/binary ).
-define(Ebinary(X),      ?STRING(X) ).

%% Cipher details
-define(SSH_CIPHER_NONE, 0).
-define(SSH_CIPHER_3DES, 3).
-define(SSH_CIPHER_AUTHFILE, ?SSH_CIPHER_3DES).

%% Option access macros
-define(do_get_opt(C,K,O),   ssh_options:get_value(C,K,O,  ?MODULE,?LINE)).
-define(do_get_opt(C,K,O,D), ssh_options:get_value(C,K,O,?LAZY(D),?MODULE,?LINE)).

-define(LAZY(D), fun()-> D end).

-define(GET_OPT(Key,Opts),              ?do_get_opt(user_options,    Key,Opts    ) ).
-define(GET_OPT(Key,Opts,Def),          ?do_get_opt(user_options,    Key,Opts,Def) ).
-define(GET_INTERNAL_OPT(Key,Opts),     ?do_get_opt(internal_options,Key,Opts    ) ).
-define(GET_INTERNAL_OPT(Key,Opts,Def), ?do_get_opt(internal_options,Key,Opts,Def) ).
-define(GET_SOCKET_OPT(Key,Opts),       ?do_get_opt(socket_options,  Key,Opts    ) ).
-define(GET_SOCKET_OPT(Key,Opts,Def),   ?do_get_opt(socket_options,  Key,Opts,Def) ).

-define(do_put_opt(C,KV,O),  ssh_options:put_value(C,KV,O, ?MODULE,?LINE)).

-define(PUT_OPT(KeyVal,Opts),           ?do_put_opt(user_options,    KeyVal,Opts) ).
-define(PUT_INTERNAL_OPT(KeyVal,Opts),  ?do_put_opt(internal_options,KeyVal,Opts) ).
-define(PUT_SOCKET_OPT(KeyVal,Opts),    ?do_put_opt(socket_options,  KeyVal,Opts) ).

-define(do_del_opt(C,K,O),  ssh_options:delete_key(C,K,O, ?MODULE,?LINE)).
-define(DELETE_INTERNAL_OPT(Key,Opts),  ?do_del_opt(internal_options,Key,Opts) ).


%% Types
-type role()                  :: client | server .

-type host()                  :: string() | inet:ip_address() | loopback .
-type open_socket()           :: gen_tcp:socket().

-type subsystem_spec()        :: {Name::string(), mod_args()} .
                              
-type algs_list()             :: list( alg_entry() ).
-type alg_entry()             :: {kex, [kex_alg()]} 
                               | {public_key, [pubkey_alg()]}
                               | {cipher, double_algs(cipher_alg())}
                               | {mac, double_algs(mac_alg())}
                               | {compression, double_algs(compression_alg())} .

-type kex_alg()          :: 'diffie-hellman-group-exchange-sha1' |
                            'diffie-hellman-group-exchange-sha256' |
                            'diffie-hellman-group1-sha1' |
                            'diffie-hellman-group14-sha1' |
                            'diffie-hellman-group14-sha256' |
                            'diffie-hellman-group16-sha512' |
                            'diffie-hellman-group18-sha512' |
                            'curve25519-sha256' |
                            'curve25519-sha256@libssh.org' |
                            'curve448-sha512' |
                            'ecdh-sha2-nistp256' |
                            'ecdh-sha2-nistp384' |
                            'ecdh-sha2-nistp521'
                            .

-type pubkey_alg()       :: 'ecdsa-sha2-nistp256' |
                            'ecdsa-sha2-nistp384' |
                            'ecdsa-sha2-nistp521' |
                            'ssh-ed25519'  |
                            'ssh-ed448'  |
                            'rsa-sha2-256' |
                            'rsa-sha2-512' |
                            'ssh-dss' |
                            'ssh-rsa'
                            .

-type cipher_alg()       :: '3des-cbc' |
                            'AEAD_AES_128_GCM' |
                            'AEAD_AES_256_GCM' |
                            'aes128-cbc' |
                            'aes128-ctr' |
                            'aes128-gcm@openssh.com' |
                            'aes192-ctr' |
                            'aes192-cbc' |
                            'aes256-cbc' |
                            'aes256-ctr' |
                            'aes256-gcm@openssh.com' |
                            'chacha20-poly1305@openssh.com'
                            .

-type mac_alg()          :: 'AEAD_AES_128_GCM' |
                            'AEAD_AES_256_GCM' |
                            'hmac-sha1' |
                            'hmac-sha1-96' |
                            'hmac-sha2-256' |
                            'hmac-sha2-512'
                            .

-type compression_alg()  :: 'none' |
                            'zlib' |
                            'zlib@openssh.com'
                            .

-type double_algs(AlgType)  :: list( {client2server,[AlgType]} | {server2client,[AlgType]} )
                             | [AlgType].

-type modify_algs_list()      :: list( {append,algs_list()} | {prepend,algs_list()} | {rm,algs_list()} ) .

-type internal_options()      :: ssh_options:private_options().
-type socket_options()        :: [gen_tcp:connect_option() | gen_tcp:listen_option()].
                              
-type client_options()        :: [ client_option() ] .
-type daemon_options()        :: [ daemon_option() ].
                              

-type common_options() :: [ common_option() ].
-type common_option() :: 
        ssh_file:user_dir_common_option()
      | profile_common_option()
      | max_idle_time_common_option()
      | key_cb_common_option()
      | disconnectfun_common_option()
      | unexpectedfun_common_option()
      | ssh_msg_debug_fun_common_option()
      | rekey_limit_common_option()
      | id_string_common_option()
      | pref_public_key_algs_common_option()
      | preferred_algorithms_common_option()
      | modify_algorithms_common_option()
      | auth_methods_common_option()
      | inet_common_option()
      | fd_common_option()
        .

-define(COMMON_OPTION, common_option()).

-type profile_common_option()       :: {profile,   atom() }.
-type max_idle_time_common_option() :: {idle_time, timeout()}.
-type rekey_limit_common_option()   :: {rekey_limit, Bytes::limit_bytes() |
                                                     {Minutes::limit_time(), Bytes::limit_bytes()}
                                       }.

-type limit_bytes() :: non_neg_integer() | infinity .  % non_neg_integer due to compatibility
-type limit_time()  :: pos_integer() | infinity .

-type key_cb_common_option()            :: {key_cb,  Module::atom() | {Module::atom(),Opts::[term()]} } .
-type disconnectfun_common_option()     ::
        {disconnectfun, fun((Reason::term()) -> void | any()) }.
-type unexpectedfun_common_option()     ::
        {unexpectedfun, fun((Message::term(),{Host::term(),Port::term()}) -> report | skip ) }.
-type ssh_msg_debug_fun_common_option() ::
        {ssh_msg_debug_fun, fun((ssh:connection_ref(),AlwaysDisplay::boolean(),Msg::binary(),LanguageTag::binary()) -> any()) } .

-type id_string_common_option()           :: {id_string,  string() | random | {random,Nmin::pos_integer(),Nmax::pos_integer()} }.
-type pref_public_key_algs_common_option() :: {pref_public_key_algs, [pubkey_alg()] } .
-type preferred_algorithms_common_option():: {preferred_algorithms, algs_list()}.
-type modify_algorithms_common_option()   :: {modify_algorithms,    modify_algs_list()}.
-type auth_methods_common_option()        :: {auth_methods,         string() }.

-type inet_common_option() :: {inet, inet | inet6} .
-type fd_common_option() :: {fd, gen_tcp:socket()} .


-type opaque_common_options() ::
        {transport, {atom(),atom(),atom()} }
      | {vsn, {non_neg_integer(),non_neg_integer()} }
      | {tstflg, list(term())}
      | ssh_file:user_dir_fun_common_option()
      | {max_random_length_padding, non_neg_integer()} .



-type client_option()         ::
        ssh_file:pubkey_passphrase_client_options()
      | host_accepting_client_options()
      | authentication_client_options()
      | diffie_hellman_group_exchange_client_option()
      | connect_timeout_client_option()
      | recv_ext_info_client_option()
      | opaque_client_options()
      | gen_tcp:connect_option()
      | ?COMMON_OPTION .

-type opaque_client_options() ::
        {keyboard_interact_fun, fun((Name::iodata(),
                                     Instruction::iodata(),
                                     Prompts::[{Prompt::iodata(),Echo::boolean()}]
                                    ) ->
                                      [Response::iodata()]
                                   )} 
        | opaque_common_options().

-type host_accepting_client_options() ::
        {silently_accept_hosts, accept_hosts()}
      | {user_interaction,     boolean()}
      | {save_accepted_host,   boolean()}
      | {quiet_mode,           boolean()} .

-type accept_hosts() :: boolean() 
                      | accept_callback()
                      | {HashAlgoSpec::fp_digest_alg(), accept_callback()}.

-type fp_digest_alg() :: 'md5' | crypto:sha1() | crypto:sha2() .

-type accept_callback() :: fun((PeerName::string(), fingerprint() ) -> boolean()) .
-type fingerprint() :: string() | [string()].

-type authentication_client_options() ::
        {user,                 string()}
      | {password,             string()} .

-type diffie_hellman_group_exchange_client_option() ::
        {dh_gex_limits,        {Min::pos_integer(), I::pos_integer(), Max::pos_integer()} } .

-type connect_timeout_client_option() :: {connect_timeout, timeout()} .

-type recv_ext_info_client_option() :: {recv_ext_info, boolean()} .



-type daemon_option()         ::
        subsystem_daemon_option()
      | shell_daemon_option()
      | exec_daemon_option()
      | ssh_cli_daemon_option()
      | authentication_daemon_options()
      | diffie_hellman_group_exchange_daemon_option()
      | negotiation_timeout_daemon_option()
      | hardening_daemon_options()
      | callbacks_daemon_options()
      | send_ext_info_daemon_option()
      | opaque_daemon_options()
      | gen_tcp:listen_option()
      | ?COMMON_OPTION .

-type subsystem_daemon_option() :: {subsystems, subsystem_specs()}.
-type subsystem_specs() :: [ subsystem_spec() ].

-type shell_daemon_option()     :: {shell, shell_spec()} .
-type shell_spec() :: mod_fun_args() | shell_fun() | disabled .
-type shell_fun() :: 'shell_fun/1'()  | 'shell_fun/2'() .
-type 'shell_fun/1'() :: fun((User::string()) -> pid()) .
-type 'shell_fun/2'() :: fun((User::string(),  PeerAddr::inet:ip_address()) -> pid()).

-type exec_daemon_option()      :: {exec, exec_spec()} .
-type exec_spec()               :: {direct, exec_fun()} | disabled | deprecated_exec_opt().
-type exec_fun()                :: 'exec_fun/1'() | 'exec_fun/2'() | 'exec_fun/3'().
-type 'exec_fun/1'() :: fun((Cmd::string()) -> exec_result()) .
-type 'exec_fun/2'() :: fun((Cmd::string(), User::string()) -> exec_result()) .
-type 'exec_fun/3'() :: fun((Cmd::string(), User::string(), ClientAddr::ip_port()) -> exec_result()) .
-type exec_result()  :: {ok,Result::term()} | {error,Reason::term()} .
-type deprecated_exec_opt() :: fun() | mod_fun_args() .

-type ssh_cli_daemon_option()   :: {ssh_cli, mod_args() | no_cli }.

-type send_ext_info_daemon_option() :: {send_ext_info, boolean()} .

-type authentication_daemon_options() ::
        ssh_file:system_dir_daemon_option()
      | {auth_method_kb_interactive_data, prompt_texts() }
      | {user_passwords, [{UserName::string(),Pwd::string()}]}
      | {password, string()}
      | {pwdfun, pwdfun_2() | pwdfun_4()} .

-type prompt_texts() ::
        kb_int_tuple()
      | kb_int_fun_3()
      .

-type kb_int_fun_3() :: fun((Peer::ip_port(), User::string(), Service::string()) -> kb_int_tuple()).
-type kb_int_tuple() :: {Name::string(), Instruction::string(), Prompt::string(), Echo::boolean()}.

-type pwdfun_2() :: fun((User::string(), Password::string()) -> boolean()) .
-type pwdfun_4() :: fun((User::string(),
                         Password::string(),
                         PeerAddress::ip_port(),
                         State::any()) ->
                               boolean() | disconnect | {boolean(),NewState::any()}
                       ) .

-type diffie_hellman_group_exchange_daemon_option() ::
        {dh_gex_groups, [explicit_group()] | explicit_group_file() | ssh_moduli_file()}
      | {dh_gex_limits, {Min::pos_integer(), Max::pos_integer()} } .

-type explicit_group() :: {Size::pos_integer(),G::pos_integer(),P::pos_integer()} .
-type explicit_group_file() :: {file,string()} .
-type ssh_moduli_file() :: {ssh_moduli_file,string()}.

-type negotiation_timeout_daemon_option() :: {negotiation_timeout, timeout()} .

-type hardening_daemon_options() ::
        {max_sessions, pos_integer()}
      | {max_channels, pos_integer()}
      | {parallel_login, boolean()}
      | {minimal_remote_max_packet_size, pos_integer()}.

-type callbacks_daemon_options() ::
        {failfun, fun((User::string(), PeerAddress::inet:ip_address(), Reason::term()) -> _)}
      | {connectfun, fun((User::string(), PeerAddress::inet:ip_address(), Method::string()) ->_)} .

-type opaque_daemon_options()  ::
        {infofun, fun()}
      | opaque_common_options().

-type ip_port() :: {inet:ip_address(), inet:port_number()} .

-type mod_args() :: {Module::atom(), Args::list()} .
-type mod_fun_args() :: {Module::atom(), Function::atom(), Args::list()} .


%% Records
-record(ssh,
	{
	  role :: client | role(),
	  peer :: undefined | 
                  {inet:hostname(),ip_port()},         %% string version of peer address 

          local,        %% Local sockname. Need this AFTER a socket is closed by i.e. a crash

	  c_vsn,        %% client version {Major,Minor}
	  s_vsn,        %% server version {Major,Minor}

	  c_version,    %% client version string
	  s_version,    %% server version string

	  c_keyinit,    %% binary payload of kexinit packet
	  s_keyinit,    %% binary payload of kexinit packet

          send_ext_info, %% May send ext-info to peer
          recv_ext_info, %% Expect ext-info from peer

	  algorithms,   %% #alg{}
	  
	  send_mac = none, %% send MAC algorithm
	  send_mac_key,  %% key used in send MAC algorithm
	  send_mac_size = 0,

	  recv_mac = none, %% recv MAC algorithm
	  recv_mac_key,  %% key used in recv MAC algorithm
	  recv_mac_size = 0,

	  encrypt = none,       %% encrypt algorithm
          encrypt_cipher,       %% cipher. could be different from the algorithm
	  encrypt_keys,         %% encrypt keys
	  encrypt_block_size = 8,
	  encrypt_ctx,

	  decrypt = none,       %% decrypt algorithm
          decrypt_cipher,       %% cipher. could be different from the algorithm
	  decrypt_keys,         %% decrypt keys
	  decrypt_block_size = 8,
	  decrypt_ctx,          %% Decryption context   

	  compress = none,
	  compress_ctx,
	  decompress = none,
	  decompress_ctx,

	  c_lng=none,   %% client to server languages
	  s_lng=none,   %% server to client languages

	  user_ack    = true,   %% client
	  timeout     = infinity,

	  shared_secret,        %% K from key exchange
	  exchanged_hash,       %% H from key exchange
	  session_id,           %% same as FIRST exchanged_hash
	  
	  opts = [],
	  send_sequence = 0,
	  recv_sequence = 0,
	  keyex_key,
	  keyex_info,
	  random_length_padding = ?MAX_RND_PADDING_LEN, % From RFC 4253 section 6.
	  
	  %% User auth
	  user,
	  service,
	  userauth_quiet_mode,              %  boolean()
	  userauth_methods,                 %  list( string() )  eg ["keyboard-interactive", "password"]
	  userauth_supported_methods,       %  string() eg "keyboard-interactive,password"
          userauth_pubkeys,
	  kb_tries_left = 0,                %  integer(), num tries left for "keyboard-interactive"
	  userauth_preference,
	  available_host_keys,
	  pwdfun_user_state,
	  authenticated = false
	 }).

-record(alg,
	{
	  kex,
	  hkey,
	  send_mac,
	  recv_mac,
	  encrypt,
	  decrypt,
	  compress,
	  decompress,
	  c_lng,
	  s_lng,
          send_ext_info,
          recv_ext_info
	 }).

-record(ssh_pty, {term = "", % e.g. "xterm"
		  width = 80,
		  height = 25,
		  pixel_width = 1024,
		  pixel_height = 768,
		  modes = <<>>}).


%% dbg help macros
-define(wr_record(N,BlackList),
        wr_record(R=#N{}) ->  ssh_dbg:wr_record(R, record_info(fields,N), BlackList)
        ).

-define(wr_record(N), ?wr_record(N, [])).


%% Circular trace buffer macros

-record(circ_buf_entry,
        {
          module,
          line,
          function,
          pid = self(),
          value
        }).

-define(CIRC_BUF_IN(VALUE),
        ssh_dbg:cbuf_in(
          #circ_buf_entry{module = ?MODULE,
                          line = ?LINE,
                          function = {?FUNCTION_NAME,?FUNCTION_ARITY},
                          pid = self(),
                          value = (VALUE)
                         })
       ).

-define(CIRC_BUF_IN_ONCE(VALUE),
        ((fun(V) -> ?CIRC_BUF_IN(V), V end)(VALUE))
       ).
                 
-endif. % SSH_HRL defined
