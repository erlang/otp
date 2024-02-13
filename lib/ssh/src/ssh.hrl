%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2024. All Rights Reserved.
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

-define(DEFAULT_TIMEOUT, 5000).

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
-define(STRING(X),   ?UINT32((byte_size(X))), (X)/binary).

-define(DEC_BIN(X,Len),   ?UINT32(Len), X:Len/binary ).
-define(DEC_INT(I,Len),   ?UINT32(Len), I:Len/big-signed-integer-unit:8 ).
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

%% Other macros
-define(to_binary(X), (try iolist_to_binary(X) catch _:_ -> unicode:characters_to_binary(X) end) ).

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

-doc(#{title => <<"Other data types">>}).
-type host()                  :: string() | inet:ip_address() | loopback .
-doc """
The socket is supposed to be result of a [gen_tcp:connect](`gen_tcp:connect/3`)
or a [gen_tcp:accept](`gen_tcp:accept/1`). The socket must be in passive mode
(that is, opened with the option `{active,false})`.
""".
-doc(#{title => <<"Other data types">>}).
-type open_socket()           :: gen_tcp:socket().

-doc """
Defines a subsystem in the daemon.

The `subsystem_name` is the name that a client requests to start with for
example `ssh_connection:subsystem/4`.

The `channel_callback` is the module that implements the `m:ssh_server_channel`
(replaces ssh_daemon_channel) behaviour in the daemon. See the section
[Creating a Subsystem](using_ssh.md#usersguide_creating_a_subsystem) in the
User's Guide for more information and an example.

If the subsystems option is not present, the value of
`ssh_sftpd:subsystem_spec([])` is used. This enables the sftp subsystem by
default. The option can be set to the empty list if you do not want the daemon
to run any subsystems.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type subsystem_spec()        :: {Name::string(), mod_args()} .
                              
-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
-type algs_list()             :: list( alg_entry() ).
-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
-type alg_entry()             :: {kex, [kex_alg()]} 
                               | {public_key, [pubkey_alg()]}
                               | {cipher, double_algs(cipher_alg())}
                               | {mac, double_algs(mac_alg())}
                               | {compression, double_algs(compression_alg())} .

-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
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

-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
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

-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
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

-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
-type mac_alg()          :: 'AEAD_AES_128_GCM' |
                            'AEAD_AES_256_GCM' |
                            'hmac-sha1' |
                            'hmac-sha1-etm@openssh.com' |
                            'hmac-sha1-96' |
                            'hmac-sha2-256' |
                            'hmac-sha2-512' |
                            'hmac-sha2-256-etm@openssh.com' |
                            'hmac-sha2-512-etm@openssh.com'
                            .

-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
-type compression_alg()  :: 'none' |
                            'zlib' |
                            'zlib@openssh.com'
                            .

-doc """
List of algorithms to use in the algorithm negotiation. The default
`t:algs_list/0` can be obtained from `default_algorithms/0`.

If an alg_entry() is missing in the algs_list(), the default value is used for
that entry.

Here is an example of this option:

```erlang
	  {preferred_algorithms,
	  [{public_key,['ssh-rsa','ssh-dss']},
	  {cipher,[{client2server,['aes128-ctr']},
          {server2client,['aes128-cbc','3des-cbc']}]},
	  {mac,['hmac-sha2-256','hmac-sha1']},
	  {compression,[none,zlib]}
	  ]
	  }
```

The example specifies different algorithms in the two directions (client2server
and server2client), for cipher but specifies the same algorithms for mac and
compression in both directions. The kex (key exchange) is implicit but
public_key is set explicitly.

For background and more examples see the
[User's Guide](configure_algos.md#introduction).

If an algorithm name occurs more than once in a list, the behaviour is
undefined. The tags in the property lists are also assumed to occur at most one
time.

> #### Warning {: .warning }
>
> Changing the values can make a connection less secure. Do not change unless
> you know exactly what you are doing. If you do not understand the values then
> you are not supposed to change them.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type double_algs(AlgType)  :: list( {client2server,[AlgType]} | {server2client,[AlgType]} )
                             | [AlgType].

-doc """
Modifies the list of algorithms to use in the algorithm negotiation. The
modifications are applied after the option `preferred_algorithms` (if existing)
is applied.

The algorithm for modifications works like this:

- Input is the `t:modify_algs_list/0` and a set of algorithms `A` obtained from
  the `preferred_algorithms` option if existing, or else from the
  [ssh:default_algorithms/0](`default_algorithms/0`).
- The head of the `t:modify_algs_list/0` modifies `A` giving the result `A'`.

  The possible modifications are:

  - Append or prepend supported but not enabled algorithm(s) to the list of
    algorithms. If the wanted algorithms already are in `A` they will first be
    removed and then appended or prepended,
  - Remove (rm) one or more algorithms from `A`.

- Repeat the modification step with the tail of `t:modify_algs_list/0` and the
  resulting `A'`.

If an unsupported algorithm is in the `t:modify_algs_list/0`, it will be
silently ignored

If there are more than one modify_algorithms options, the result is undefined.

Here is an example of this option:

```text
	  {modify_algorithms,
	  [{prepend, [{kex, ['diffie-hellman-group1-sha1']}],
	  {rm,      [{compression, [none]}]}
	  ]
	  }
```

The example specifies that:

- the old key exchange algorithm 'diffie-hellman-group1-sha1' should be the main
  alternative. It will be the main alternative since it is prepened to the list
- The compression algorithm none (= no compression) is removed so compression is
  enforced

For background and more examples see the
[User's Guide](configure_algos.md#introduction).
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type modify_algs_list()      :: list( {append,algs_list()} | {prepend,algs_list()} | {rm,algs_list()} ) .

-type internal_options()      :: ssh_options:private_options().
-type socket_options()        :: [gen_tcp:connect_option() | gen_tcp:listen_option()].
                              
-doc(#{title => <<"Client Options">>,equiv => {type,client_option,0}}).
-type client_options()        :: [ client_option() ] .
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,daemon_option,0}}).
-type daemon_options()        :: [ daemon_option() ].
                              

-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,common_option,0}}).
-type common_options() :: [ common_option() ].
-doc """
The options above can be used both in clients and in daemons (servers). They are
further explained below.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type common_option() :: 
        ssh_file:user_dir_common_option()
      | profile_common_option()
      | max_idle_time_common_option()
      | max_log_item_len_common_option()
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

-doc """
Used together with `ip-address` and `port` to uniquely identify a ssh daemon.
This can be useful in a virtualized environment, where there can be more that
one server that has the same `ip-address` and `port`. If this property is not
explicitly set, it is assumed that the the `ip-address` and `port` uniquely
identifies the SSH daemon.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type profile_common_option()       :: {profile,   atom() }.
-doc """
Sets a time-out on a connection when no channels are open. Defaults to
`infinity`. The unit is milliseconds.

The timeout is not active until channels are started, so it does not limit the
time from the connection creation to the first channel opening.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type max_idle_time_common_option() :: {idle_time, timeout()}.
-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,limit_time,0}}).
-type rekey_limit_common_option()   :: {rekey_limit, Bytes::limit_bytes() |
                                                     {Minutes::limit_time(), Bytes::limit_bytes()}
                                       }.
-doc """
Sets a limit for the size of a logged item excluding a header. The unit is bytes
and the value defaults to 500.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type max_log_item_len_common_option() :: {max_log_item_len, limit_bytes()} .

-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,limit_time,0}}).
-type limit_bytes() :: non_neg_integer() | infinity .  % non_neg_integer due to compatibility
-doc """
Sets the limit when rekeying is to be initiated. Both the max time and max
amount of data could be configured:

- `{Minutes, Bytes}` initiate rekeying when any of the limits are reached.
- `Bytes` initiate rekeying when `Bytes` number of bytes are transferred, or at
  latest after one hour.

When a rekeying is done, both the timer and the byte counter are restarted.
Defaults to one hour and one GByte.

If `Minutes` is set to `infinity`, no rekeying will ever occur due to that max
time has passed. Setting `Bytes` to `infinity` will inhibit rekeying after a
certain amount of data has been transferred. If the option value is set to
`{infinity, infinity}`, no rekeying will be initiated. Note that rekeying
initiated by the peer will still be performed.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type limit_time()  :: pos_integer() | infinity .

-doc """
Module implementing the behaviour `m:ssh_client_key_api` and/or
`m:ssh_server_key_api`. Can be used to customize the handling of public keys. If
callback options are provided along with the module name, they are made
available to the callback module via the options passed to it under the key
'key_cb_private'.

The `Opts` defaults to `[]` when only the `Module` is specified.

The default value of this option is `{ssh_file, []}`. See also the manpage of
`m:ssh_file`.

A call to the call-back function `F` will be

```text
	  Module:F(..., [{key_cb_private,Opts}|UserOptions])
```

where `...` are arguments to `F` as in `m:ssh_client_key_api` and/or
`m:ssh_server_key_api`. The `UserOptions` are the options given to
[ssh:connect](`connect/3`), [ssh:shell](`shell/1`) or [ssh:daemon](`daemon/2`).
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type key_cb_common_option()            :: {key_cb,  Module::atom() | {Module::atom(),Opts::[term()]} } .
-doc "Provides a fun to implement your own logging or other handling at disconnects.".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type disconnectfun_common_option()     ::
        {disconnectfun, fun((Reason::term()) -> void | any()) }.
-doc """
Provides a fun to implement your own logging or other action when an unexpected
message arrives. If the fun returns `report` the usual info report is issued but
if `skip` is returned no report is generated.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type unexpectedfun_common_option()     ::
        {unexpectedfun, fun((Message::term(),{Host::term(),Port::term()}) -> report | skip ) }.
-doc """
Provide a fun to implement your own logging of the SSH message SSH_MSG_DEBUG.
The last three parameters are from the message, see
[RFC 4253, section 11.3](https://tools.ietf.org/html/rfc4253#section-11.3). The
`t:connection_ref/0` is the reference to the connection on which the message
arrived. The return value from the fun is not checked.

The default behaviour is ignore the message. To get a printout for each message
with `AlwaysDisplay = true`, use for example
`{ssh_msg_debug_fun, fun(_,true,M,_)-> io:format("DEBUG: ~p~n", [M]) end}`
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type ssh_msg_debug_fun_common_option() ::
        {ssh_msg_debug_fun, fun((ssh:connection_ref(),AlwaysDisplay::boolean(),Msg::binary(),LanguageTag::binary()) -> any()) } .

-doc """
The string the daemon will present to a connecting peer initially. The default
value is "Erlang/VSN" where VSN is the ssh application version number.

The value `random` will cause a random string to be created at each connection
attempt. This is to make it a bit more difficult for a malicious peer to find
the ssh software brand and version.

The value `{random, Nmin, Nmax}` will make a random string with at least `Nmin`
characters and at most `Nmax` characters.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type id_string_common_option()           :: {id_string,  string() | random | {random,Nmin::pos_integer(),Nmax::pos_integer()} }.
-doc """
List of user (client) public key algorithms to try to use.

The default value is the `public_key` entry in the list returned by
[ssh:default_algorithms/0](`default_algorithms/0`).

If there is no public key of a specified type available, the corresponding entry
is ignored. Note that the available set is dependent on the underlying cryptolib
and current user's public keys.

See also the option [`user_dir`](`t:ssh_file:user_dir_common_option/0`) for
specifying the path to the user's keys.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type pref_public_key_algs_common_option() :: {pref_public_key_algs, [pubkey_alg()] } .
-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,double_algs,1}}).
-type preferred_algorithms_common_option():: {preferred_algorithms, algs_list()}.
-doc(#{title => <<"Options common to clients and daemons">>,
       equiv => {type,modify_algs_list,0}}).
-type modify_algorithms_common_option()   :: {modify_algorithms,    modify_algs_list()}.
-doc """
Comma-separated string that determines which authentication methods that the
client shall support and in which order they are tried. Defaults to
`"publickey,keyboard-interactive,password"`

Note that the client is free to use any order and to exclude methods.
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type auth_methods_common_option()        :: {auth_methods,         string() }.

-doc "IP version to use when the host address is specified as `any`.".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type inet_common_option() :: {inet, inet | inet6} .
-doc """
Allows an existing file-descriptor to be used (passed on to the transport
protocol).
""".
-doc(#{title => <<"Options common to clients and daemons">>}).
-type fd_common_option() :: {fd, gen_tcp:socket()} .


-doc """
Opaque types that define experimental options that are not to be used in
products.
""".
-doc(#{title => <<"Other data types">>}).
-type opaque_common_options() ::
        {transport, {atom(),atom(),atom()} }
      | {vsn, {non_neg_integer(),non_neg_integer()} }
      | {tstflg, list(term())}
      | ssh_file:user_dir_fun_common_option()
      | {max_random_length_padding, non_neg_integer()} .



-doc """
Options for [clients](`connect/3`). The individual options are further explained
below or by following the hyperlinks.

Note that not every `t:gen_tcp:connect_option/0` is accepted. See
`set_sock_opts/2` for a list of prohibited options.

Also note that setting a `t:gen_tcp:connect_option/0` could change the socket in
a way that impacts the ssh client's behaviour negatively. You use it on your own
risk.
""".
-doc(#{title => <<"Client Options">>}).
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

-doc(#{title => <<"Other data types">>,
       equiv => {type,opaque_common_options,0}}).
-type opaque_client_options() ::
        {keyboard_interact_fun, fun((Name::iodata(),
                                     Instruction::iodata(),
                                     Prompts::[{Prompt::iodata(),Echo::boolean()}]
                                    ) ->
                                      [Response::iodata()]
                                   )} 
        | opaque_common_options().

-doc(#{title => <<"Client Options">>,equiv => {type,fingerprint,0}}).
-type host_accepting_client_options() ::
        {silently_accept_hosts, accept_hosts()}
      | {user_interaction,     boolean()}
      | {save_accepted_host,   boolean()}
      | {quiet_mode,           boolean()} .

-doc(#{title => <<"Client Options">>,equiv => {type,fingerprint,0}}).
-type accept_hosts() :: boolean() 
                      | accept_callback()
                      | {HashAlgoSpec::fp_digest_alg(), accept_callback()}.

-doc(#{title => <<"Client Options">>,equiv => {type,fingerprint,0}}).
-type fp_digest_alg() :: 'md5' | crypto:sha1() | crypto:sha2() .

-doc(#{title => <<"Client Options">>,equiv => {type,fingerprint,0}}).
-type accept_callback() :: fun((PeerName::string(), fingerprint() ) -> boolean()) % Old style
                         | fun((PeerName::string(), Port::inet:port_number(), fingerprint() ) -> boolean()) % New style
                           .
-doc """
- **`silently_accept_hosts`{: #hardening_client_options-silently_accept_hosts
  }** - This option guides the `connect` function on how to act when the
  connected server presents a Host Key that the client has not seen before. The
  default is to ask the user with a question on stdio of whether to accept or
  reject the new Host Key. See the option
  [`user_dir`](`t:ssh_file:user_dir_common_option/0`) for specifying the path to
  the file `known_hosts` where previously accepted Host Keys are recorded. See
  also the option [key_cb](`t:key_cb_common_option/0`) for the general way to
  handle keys.

  The option can be given in three different forms as seen
  [above](`t:accept_hosts/0`):

  - The value is a `t:boolean/0`. The value `true` will make the client accept
    any unknown Host Key without any user interaction. The value `false`
    preserves the default behaviour of asking the user on stdio.
  - An `t:accept_callback/0` will be called and the boolean return value `true`
    will make the client accept the Host Key. A return value of `false` will
    make the client to reject the Host Key and as a result the connection will
    be closed. The arguments to the fun are:
    - `PeerName` \- a string with the name or address of the remote host.
    - `FingerPrint` \- the fingerprint of the Host Key as
      `hostkey_fingerprint/1` calculates it.
  - A tuple `{HashAlgoSpec, accept_callback}`. The `HashAlgoSpec` specifies
    which hash algorithm shall be used to calculate the fingerprint used in the
    call of the `t:accept_callback/0`. The `HashALgoSpec` is either an atom or a
    list of atoms as the first argument in `hostkey_fingerprint/2`. If it is a
    list of hash algorithm names, the `FingerPrint` argument in the
    `t:accept_callback/0` will be a list of fingerprints in the same order as
    the corresponding name in the `HashAlgoSpec` list.

- **`user_interaction`** - If `false`, disables the client to connect to the
  server if any user interaction is needed, such as accepting the server to be
  added to the `known_hosts` file, or supplying a password.

  Even if user interaction is allowed it can be suppressed by other options,
  such as `silently_accept_hosts` and `password`. However, those options are not
  always desirable to use from a security point of view.

  Defaults to `true`.

- **`save_accepted_host`** - If `true`, the client saves an accepted host key to
  avoid the accept question the next time the same host is connected. If the
  option [`key_cb`](`t:key_cb_common_option/0`) is not present, the key is saved
  in the file "known_hosts". See option
  [`user_dir`](`t:ssh_file:user_dir_common_option/0`) for the location of that
  file.

  If `false`, the key is not saved and the key will still be unknown at the next
  access of the same host.

  Defaults to `true`

- **`quiet_mode`** - If `true`, the client does not print anything on
  authorization.

  Defaults to `false`
""".
-doc(#{title => <<"Client Options">>}).
-type fingerprint() :: string() | [string()].

-doc """
- **`user`** - Provides the username. If this option is not given, `ssh` reads
  from the environment (`LOGNAME` or `USER` on UNIX, `USERNAME` on Windows).

- **`password`** - Provides a password for password authentication. If this
  option is not given, the user is asked for a password, if the password
  authentication method is attempted.
""".
-doc(#{title => <<"Client Options">>}).
-type authentication_client_options() ::
        {user,                 string()}
      | {password,             string()} .

-doc """
Sets the three diffie-hellman-group-exchange parameters that guides the
connected server in choosing a group. See
[RFC 4419](https://tools.ietf.org/html/rfc4419) for the details. The default
value is `{1024, 6144, 8192}`.
""".
-doc(#{title => <<"Client Options">>}).
-type diffie_hellman_group_exchange_client_option() ::
        {dh_gex_limits,        {Min::pos_integer(), I::pos_integer(), Max::pos_integer()} } .

-doc """
Sets a timeout on the transport layer connect time. For `m:gen_tcp` the time is
in milli-seconds and the default value is `infinity`.

See the parameter `Timeout` in `connect/4` for a timeout of the negotiation
phase.
""".
-doc(#{title => <<"Client Options">>}).
-type connect_timeout_client_option() :: {connect_timeout, timeout()} .

-doc """
Make the client tell the server that the client accepts extension negotiation,
that is, include `ext-info-c` in the kexinit message sent. See
[RFC 8308](https://tools.ietf.org/html/rfc8308) for details and
[ssh(6)](ssh_app.md#supported-ext-info) for a list of currently implemented
extensions.

Default value is `true` which is compatible with other implementations not
supporting ext-info.
""".
-doc(#{title => <<"Client Options">>}).
-type recv_ext_info_client_option() :: {recv_ext_info, boolean()} .



-doc """
Options for [daemons](`daemon/1`). The individual options are further explained
below or by following the hyperlinks.

Note that not every `t:gen_tcp:listen_option/0` is accepted. See
`set_sock_opts/2` for a list of prohibited options.

Also note that setting a `t:gen_tcp:listen_option/0` could change the socket in
a way that impacts the ssh deamon's behaviour negatively. You use it on your own
risk.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type daemon_option()         ::
        subsystem_daemon_option()
      | shell_daemon_option()
      | exec_daemon_option()
      | ssh_cli_daemon_option()
      | tcpip_tunnel_out_daemon_option()
      | tcpip_tunnel_in_daemon_option()
      | authentication_daemon_options()
      | diffie_hellman_group_exchange_daemon_option()
      | max_initial_idle_time_daemon_option()
      | negotiation_timeout_daemon_option()
      | hello_timeout_daemon_option()
      | hardening_daemon_options()
      | callbacks_daemon_options()
      | send_ext_info_daemon_option()
      | opaque_daemon_options()
      | gen_tcp:listen_option()
      | ?COMMON_OPTION .

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,subsystem_spec,0}}).
-type subsystem_daemon_option() :: {subsystems, subsystem_specs()}.
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,subsystem_spec,0}}).
-type subsystem_specs() :: [ subsystem_spec() ].

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,'shell_fun/2',0}}).
-type shell_daemon_option()     :: {shell, shell_spec()} .
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,'shell_fun/2',0}}).
-type shell_spec() :: mod_fun_args() | shell_fun() | disabled .
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,'shell_fun/2',0}}).
-type shell_fun() :: 'shell_fun/1'()  | 'shell_fun/2'() .
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,'shell_fun/2',0}}).
-type 'shell_fun/1'() :: fun((User::string()) -> pid()) .
-doc """
Defines the read-eval-print loop used in a daemon when a shell is requested by
the client. The default is to use the Erlang shell: `{shell, start, []}`

See the option [`exec-option`](`t:exec_daemon_option/0`) for a description of
how the daemon executes shell-requests and exec-requests depending on the shell-
and exec-options.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type 'shell_fun/2'() :: fun((User::string(),  PeerAddr::inet:ip_address()) -> pid()).

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,exec_spec,0}}).
-type exec_daemon_option()      :: {exec, exec_spec()} .
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type exec_spec()               :: {direct, exec_fun()} | disabled | deprecated_exec_opt().
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type exec_fun()                :: 'exec_fun/1'() | 'exec_fun/2'() | 'exec_fun/3'().
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,'exec_fun/3',0}}).
-type 'exec_fun/1'() :: fun((Cmd::string()) -> exec_result()) .
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,'exec_fun/3',0}}).
-type 'exec_fun/2'() :: fun((Cmd::string(), User::string()) -> exec_result()) .
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type 'exec_fun/3'() :: fun((Cmd::string(), User::string(), ClientAddr::ip_port()) -> exec_result()) .
-doc """
This option changes how the daemon executes exec-requests from clients. The term
in the return value is formatted to a string if it is a non-string type. No
trailing newline is added in the ok-case.

See the User's Guide section on
[One-Time Execution](using_ssh.md#one-time-execution) for examples.

Error texts are returned on channel-type 1 which usually is piped to `stderr` on
e.g Linux systems. Texts from a successful execution are returned on
channel-type 0 and will in similar manner be piped to `stdout`. The exit-status
code is set to 0 for success and 255 for errors. The exact results presented on
the client side depends on the client and the client's operating system.

In case of the `{direct, exec_fun()}` variant or no exec-option at all, all
reads from `standard_input` will be from the received data-events of type 0.
Those are sent by the client. Similarly all writes to `standard_output` will be
sent as data-events to the client. An OS shell client like the command 'ssh'
will usually use stdin and stdout for the user interface.

The option cooperates with the daemon-option
[`shell`](`t:shell_daemon_option/0`) in the following way:

- **1\. If neither the [`exec-option`](`t:exec_daemon_option/0`) nor the
  [`shell-option`](`t:shell_daemon_option/0`) is present:** - The default Erlang
  evaluator is used both for exec and shell requests. The result is returned to
  the client.

- **2\. If the [`exec_spec`](`t:exec_daemon_option/0`)'s value is `disabled`
  (the [`shell-option`](`t:shell_daemon_option/0`) may or may not be
  present):** - No exec-requests are executed but shell-requests are not
  affected, they follow the [`shell_spec`](`t:shell_daemon_option/0`)'s value.

- **3\. If the [`exec-option`](`t:exec_daemon_option/0`) is present and the
  [`exec_spec`](`t:exec_daemon_option/0`) value =/= `disabled` (the
  [`shell-option`](`t:shell_daemon_option/0`) may or may not be present):** -
  The [`exec_spec`](`t:exec_daemon_option/0`) `fun()` is called with the same
  number of parameters as the arity of the fun, and the result is returned to
  the client. Shell-requests are not affected, they follow the
  [`shell_spec`](`t:shell_daemon_option/0`)'s value.

- **4\. If the [`exec-option`](`t:exec_daemon_option/0`) is absent, and the
  [`shell-option`](`t:shell_daemon_option/0`) is present with the default Erlang
  shell as the [`shell_spec`](`t:shell_daemon_option/0`)'s value:** - The
  default Erlang evaluator is used both for exec and shell requests. The result
  is returned to the client.

- **5\. If the [`exec-option`](`t:exec_daemon_option/0`) is absent, and the
  [`shell-option`](`t:shell_daemon_option/0`) is present with a value that is
  neither the default Erlang shell nor the value `disabled`:** - The
  exec-request is not evaluated and an error message is returned to the client.
  Shell-requests are executed according to the value of the
  [`shell_spec`](`t:shell_daemon_option/0`).

- **6\. If the [`exec-option`](`t:exec_daemon_option/0`) is absent, and the
  [`shell_spec`](`t:shell_daemon_option/0`)'s value is `disabled`:** - Exec
  requests are executed by the default shell, but shell-requests are not
  executed.

If a custom CLI is installed (see the option
[`ssh_cli`](`t:ssh_cli_daemon_option/0`)) the rules above are replaced by thoose
implied by the custom CLI.

> #### Note {: .info }
>
> The [`exec-option`](`t:exec_daemon_option/0`) has existed for a long time but
> has not previously been documented. The old definition and behaviour are
> retained but obey the rules 1-6 above if conflicting. The old and undocumented
> style should not be used in new programs.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type exec_result()  :: {ok,Result::term()} | {error,Reason::term()} .
-doc """
Old-style exec specification that are kept for compatibility, but should not be
used in new programs
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type deprecated_exec_opt() :: fun() | mod_fun_args() .

-doc """
Provides your own CLI implementation in a daemon.

It is a channel callback module that implements a shell and command execution.
The shell's read-eval-print loop can be customized, using the option
[`shell`](`t:shell_daemon_option/0`). This means less work than implementing an
own CLI channel. If `ssh_cli` is set to `no_cli`, the CLI channels like
[`shell`](`t:shell_daemon_option/0`) and [`exec`](`t:exec_daemon_option/0`) are
disabled and only subsystem channels are allowed.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type ssh_cli_daemon_option()   :: {ssh_cli, mod_args() | no_cli }.

-doc """
Enables (`true`) or disables (`false`) the possibility to tunnel a TCP/IP
connection out of a [server](`daemon/2`). Disabled per default.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type tcpip_tunnel_out_daemon_option() :: {tcpip_tunnel_out, boolean()} .
-doc """
Enables (`true`) or disables (`false`) the possibility to tunnel a TCP/IP
connection in to a [server](`daemon/2`). Disabled per default.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type tcpip_tunnel_in_daemon_option() :: {tcpip_tunnel_in, boolean()} .

-doc """
Make the server (daemon) tell the client that the server accepts extension
negotiation, that is, include `ext-info-s` in the kexinit message sent. See
[RFC 8308](https://tools.ietf.org/html/rfc8308) for details and
[ssh(6)](ssh_app.md#supported-ext-info) for a list of currently implemented
extensions.

Default value is `true` which is compatible with other implementations not
supporting ext-info.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type send_ext_info_daemon_option() :: {send_ext_info, boolean()} .

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,pwdfun_4,0}}).
-type authentication_daemon_options() ::
        ssh_file:system_dir_daemon_option()
      | {auth_method_kb_interactive_data, prompt_texts() }
      | {user_passwords, [{UserName::string(),Pwd::string()}]}
      | {pk_check_user, boolean()}  
      | {password, string()}
      | {pwdfun, pwdfun_2() | pwdfun_4()}
      | {no_auth_needed, boolean()}
        .

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,pwdfun_4,0}}).
-type prompt_texts() ::
        kb_int_tuple()
      | kb_int_fun_3()
      | kb_int_fun_4()
      .

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,pwdfun_4,0}}).
-type kb_int_fun_3() :: fun((Peer::ip_port(), User::string(), Service::string()) -> kb_int_tuple()).
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,pwdfun_4,0}}).
-type kb_int_fun_4() :: fun((Peer::ip_port(), User::string(), Service::string(), State::any()) -> kb_int_tuple()).
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,pwdfun_4,0}}).
-type kb_int_tuple() :: {Name::string(), Instruction::string(), Prompt::string(), Echo::boolean()}.

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,pwdfun_4,0}}).
-type pwdfun_2() :: fun((User::string(), Password::string()|pubkey) -> boolean()) .
-doc """
- **`auth_method_kb_interactive_data`** - Sets the text strings that the daemon
  sends to the client for presentation to the user when using
  `keyboard-interactive` authentication.

  If the fun/3 or fun/4 is used, it is called when the actual authentication
  occurs and may therefore return dynamic data like time, remote ip etc.

  The parameter `Echo` guides the client about need to hide the password.

  The default value is:
  `{auth_method_kb_interactive_data, {"SSH server", "Enter password for \""++User++"\"", "password: ", false}>`

- **`user_passwords`{: #option-user_passwords }** - Provides passwords for
  password authentication. The passwords are used when someone tries to connect
  to the server and public key user-authentication fails. The option provides a
  list of valid usernames and the corresponding passwords.

  > #### Warning {: .warning }
  >
  > Note that this is very insecure due to the plain-text passwords; it is
  > intended for test purposes. Use the [`pwdfun`](`m:ssh#option-pwdfun`) option
  > to handle the password checking instead.

- **`pk_check_user`{: #option-pk_check_user }** - Enables checking of the
  [client's user name](`t:authentication_client_options/0`) in the server when
  doing public key authentication. It is disabled by default.

  The term "user" is used differently in OpenSSH and SSH in Erlang/OTP: see more
  in the [User's Guide](terminology.md#the-term-user).

  If the option is enabled, and no [`pwdfun`](`m:ssh#option-pwdfun`) is present,
  the user name must present in the
  [user_passwords](`m:ssh#option-user_passwords`) for the check to succeed but
  the value of the password is not checked.

  In case of a [`pwdfun`](`m:ssh#option-pwdfun`) checking the user, the atom
  `pubkey` is put in the password argument.

- **`password`{: #option-password }** - Provides a global password that
  authenticates any user.

  > #### Warning {: .warning }
  >
  > Intended to facilitate testing.
  >
  > From a security perspective this option makes the server very vulnerable.

- **`pwdfun`{: #option-pwdfun } with `t:pwdfun_4/0`** - Provides a function for
  password validation. This could used for calling an external system or
  handling passwords stored as hash values.

  This fun can also be used to make delays in authentication tries for example
  by calling `timer:sleep/1`.

  To facilitate for instance counting of failed tries, the `State` variable
  could be used. This state is per connection only. The first time the pwdfun is
  called for a connection, the `State` variable has the value `undefined`.

  The fun should return:

  - `true` if the user and password is valid
  - `false` if the user or password is invalid
  - `disconnect` if a SSH_MSG_DISCONNECT message should be sent immediately. It
    will be followed by a close of the underlying tcp connection.
  - `{true, NewState:any()}` if the user and password is valid
  - `{false, NewState:any()}` if the user or password is invalid

  A third usage is to block login attempts from a missbehaving peer. The `State`
  described above can be used for this. The return value `disconnect` is useful
  for this.

  In case of the [`pk_check_user`](`m:ssh#option-pk_check_user`) is set, the
  atom `pubkey` is put in the password argument when validating a public key
  login. The pwdfun is then responsible to check that the user name is valid.

- **`pwdfun` with `t:pwdfun_2/0`** - Provides a function for password
  validation. This function is called with user and password as strings, and
  returns:

  - `true` if the user and password is valid
  - `false` if the user or password is invalid

  In case of the [`pk_check_user`](`m:ssh#option-pk_check_user`) is set, the
  atom `pubkey` is put in the password argument when validating a public key
  login. The pwdfun is then responsible to check that the user name is valid.

  This variant is kept for compatibility.

- **`no_auth_needed`{: #option-no_auth_needed }** - If `true`, a client is
  authenticated without any need of providing any password or key.

  This option is only intended for very special applications due to the high
  risk of accepting any connecting client.

  The default value is `false`.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type pwdfun_4() :: fun((User::string(),
                         Password::string()|pubkey,
                         PeerAddress::ip_port(),
                         State::any()) ->
                               boolean() | disconnect | {boolean(),NewState::any()}
                       ) .

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,ssh_moduli_file,0}}).
-type diffie_hellman_group_exchange_daemon_option() ::
        {dh_gex_groups, [explicit_group()] | explicit_group_file() | ssh_moduli_file()}
      | {dh_gex_limits, {Min::pos_integer(), Max::pos_integer()} } .

-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,ssh_moduli_file,0}}).
-type explicit_group() :: {Size::pos_integer(),G::pos_integer(),P::pos_integer()} .
-doc(#{title => <<"Daemon Options (Server Options)">>,
       equiv => {type,ssh_moduli_file,0}}).
-type explicit_group_file() :: {file,string()} .
-doc """
- **`dh_gex_groups`** - Defines the groups the server may choose among when
  diffie-hellman-group-exchange is negotiated. See
  [RFC 4419](https://tools.ietf.org/html/rfc4419) for details. The three
  variants of this option are:

  - **`{Size=integer(),G=integer(),P=integer()}`** - The groups are given
    explicitly in this list. There may be several elements with the same `Size`.
    In such a case, the server will choose one randomly in the negotiated Size.

  - **`{file,filename()}`** - The file must have one or more three-tuples
    `{Size=integer(),G=integer(),P=integer()}` terminated by a dot. The file is
    read when the daemon starts.

  - **`{ssh_moduli_file,filename()}`** - The file must be in
    [ssh-keygen moduli file format](`public_key:dh_gex_group/4`). The file is
    read when the daemon starts.

  The default list is fetched from the [public_key](`public_key:dh_gex_group/4`)
  application.

- **`dh_gex_limits`** - Limits what a client can ask for in
  diffie-hellman-group-exchange. The limits will be
  `{MaxUsed = min(MaxClient,Max), MinUsed = max(MinClient,Min)}` where
  `MaxClient` and `MinClient` are the values proposed by a connecting client.

  The default value is `{0,infinity}`.

  If `MaxUsed < MinUsed` in a key exchange, it will fail with a disconnect.

  See [RFC 4419](https://tools.ietf.org/html/rfc4419) for the function of the
  Max and Min values.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type ssh_moduli_file() :: {ssh_moduli_file,string()}.

-doc """
Maximum time in milliseconds for the first channel start after completion of the
authentication negotiation. Defaults to `infinity`.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type max_initial_idle_time_daemon_option() :: {max_initial_idle_time, timeout()} .
-doc """
Maximum time in milliseconds for the authentication negotiation. Defaults to
120000 ms (2 minutes). If the client fails to log in within this time, the
connection is closed.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type negotiation_timeout_daemon_option() :: {negotiation_timeout, timeout()} .
-doc """
Maximum time in milliseconds for the first part of the ssh session setup, the
hello message exchange. Defaults to 30000 ms (30 seconds). If the client fails
to send the first message within this time, the connection is closed.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type hello_timeout_daemon_option() :: {hello_timeout, timeout()} .

-doc """
For more information about hardening, see the [Hardening](hardening.md) section
in the User's Guide chapter.

- **`max_sessions`{: #hardening_daemon_options-max_sessions }** - The maximum
  number of simultaneous sessions that are accepted at any time for this daemon.
  This includes sessions that are being authorized. Thus, if set to `N`, and `N`
  clients have connected but not started the login process, connection attempt
  `N+1` is aborted. If `N` connections are authenticated and still logged in, no
  more logins are accepted until one of the existing ones log out.

  The counter is per listening port. Thus, if two daemons are started, one with
  `{max_sessions,N}` and the other with `{max_sessions,M}`, in total `N+M`
  connections are accepted for the whole `ssh` application.

  Notice that if `parallel_login` is `false`, only one client at a time can be
  in the authentication phase.

  By default, this option is not set. This means that the number is not limited.

- **`max_channels`{: #hardening_daemon_options-max_channels }** - The maximum
  number of channels with active remote subsystem that are accepted for each
  connection to this daemon

  By default, this option is not set. This means that the number is not limited.

- **`parallel_login`{: #hardening_daemon_options-parallel_login }** - If set to
  false (the default value), only one login is handled at a time. If set to
  true, an unlimited number of login attempts are allowed simultaneously.

  If the `max_sessions` option is set to `N` and `parallel_login` is set to
  `true`, the maximum number of simultaneous login attempts at any time is
  limited to `N-K`, where `K` is the number of authenticated connections present
  at this daemon.

  > #### Warning {: .warning }
  >
  > Do not enable `parallel_logins` without protecting the server by other
  > means, for example, by the `max_sessions` option or a firewall
  > configuration. If set to `true`, there is no protection against DOS attacks.

- **`minimal_remote_max_packet_size`{:
  #hardening_daemon_options-minimal_remote_max_packet_size }** - The least
  maximum packet size that the daemon will accept in channel open requests from
  the client. The default value is 0.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type hardening_daemon_options() ::
        {max_sessions, pos_integer()}
      | {max_channels, pos_integer()}
      | {parallel_login, boolean()}
      | {minimal_remote_max_packet_size, pos_integer()}.

-doc """
- **`connectfun`** - Provides a fun to implement your own logging when a user
  authenticates to the server.

- **`failfun`** - Provides a fun to implement your own logging when a user fails
  to authenticate.
""".
-doc(#{title => <<"Daemon Options (Server Options)">>}).
-type callbacks_daemon_options() ::
        {failfun, fun((User::string(), PeerAddress::inet:ip_address(), Reason::term()) -> _)}
      | {connectfun, fun((User::string(), PeerAddress::inet:ip_address(), Method::string()) ->_)} .

-doc(#{title => <<"Other data types">>,
       equiv => {type,opaque_common_options,0}}).
-type opaque_daemon_options()  ::
        {infofun, fun()}
      | opaque_common_options().

-doc(#{title => <<"Other data types">>}).
-type ip_port() :: {inet:ip_address(), inet:port_number()} .

-doc(#{title => <<"Other data types">>}).
-type mod_args() :: {Module::atom(), Args::list()} .
-doc(#{title => <<"Other data types">>}).
-type mod_fun_args() :: {Module::atom(), Function::atom(), Args::list()} .


%% Records
-record(address, {address,
                  port,
                  profile
                 }).

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

          kex_strict_negotiated = false,

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
          recv_ext_info,
          kex_strict_negotiated = false
	 }).

-record(ssh_pty, {c_version = "", % client version string, e.g "SSH-2.0-Erlang/4.10.5"
                  term = "",      % e.g. "xterm"
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
