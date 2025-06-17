%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
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

%%% Description: SSH file handling

-module(ssh_file).
-moduledoc """
Default callback module for the client's and server's database operations in the
ssh application

This module is the default callback handler for the client's and the server's
user and host "database" operations. All data, for instance key pairs, are
stored in files in the normal file system. This page documents the files, where
they are stored and configuration options for this callback module.

The intention is to be compatible with the [OpenSSH](http://www.openssh.com)
storage in files. Therefore it mimics directories and filenames of
[OpenSSH](http://www.openssh.com).

Ssh_file implements the `m:ssh_server_key_api` and the `m:ssh_client_key_api`.
This enables the user to make an own interface using for example a database
handler.

Such another callback module could be used by setting the option
[`key_cb`](`t:ssh:key_cb_common_option/0`) when starting a client or a server
(with for example [ssh:connect](`ssh:connect/3`), [ssh:daemon](`ssh:daemon/2`)
of [ssh:shell](`ssh:shell/1`) ).

> #### Note {: .info }
>
> The functions are _Callbacks_ for the SSH app. They are not intended to be
> called from the user's code\!

## Files, directories and who uses them

### Daemons

Daemons uses all files stored in the [SYSDIR](`m:ssh_file#SYSDIR`) directory.

Optionally, in case of `publickey` authorization, one or more of the remote
user's public keys in the [USERDIR](`m:ssh_file#USERDIR`) directory are used.
See the files [`USERDIR/authorized_keys`](`m:ssh_file#FILE-authorized_keys`) and
[`USERDIR/authorized_keys2`](`m:ssh_file#FILE-authorized_keys2`).

### Clients

Clients uses all files stored in the [USERDIR](`m:ssh_file#USERDIR`) directory.

### Directory contents

- **[](){: #LOCALUSER } LOCALUSER**  
  The user name of the OS process running the Erlang virtual machine (emulator).

- **[](){: #SYSDIR } SYSDIR**  
  This is the directory holding the server's files:

  - [](){: #FILE-ssh_host_STAR_key } `ssh_host_dsa_key`{: #FILE-ssh_host_dsa_key
    } \- private dss host key (optional)
  - `ssh_host_rsa_key`{: #FILE-ssh_host_rsa_key } \- private rsa host key
    (optional)
  - `ssh_host_ecdsa_key`{: #FILE-ssh_host_ecdsa_key } \- private ecdsa host key
    (optional)
  - `ssh_host_ed25519_key`{: #FILE-ssh_host_ed25519_key } \- private eddsa host
    key for curve 25519 (optional)
  - `ssh_host_ed448_key`{: #FILE-ssh_host_ed448_key } \- private eddsa host key
    for curve 448 (optional)

  The key files could be generated with OpenSSH's ssh-keygen command.

  At least one host key must be defined. The default value of SYSDIR is
  `/etc/ssh`{: ##/etc/ssh }.

  For security reasons, this directory is normally accessible only to the root
  user.

  To change the SYSDIR, see the [system_dir](`t:system_dir_daemon_option/0`)
  option.

- **[](){: #USERDIR } USERDIR**  
  This is the directory holding the files:

  - `authorized_keys`{: #FILE-authorized_keys } and, as second alternative
    `authorized_keys2`{: #FILE-authorized_keys2 } \- the user's public keys are
    stored concatenated in one of those files.

    It is composed of lines as for
    [OpenSSH](https://man.openbsd.org/sshd#AUTHORIZED_KEYS_FILE_FORMAT):

    ```text
    (options)? keytype base64-encoded-key comment
    ```

    where

    ```text
    options :: option(,option)*
    option :: % All options are skipped
    keytype :: 'ssh-dsa'
             | 'ssh-rsa'
             | 'ssh-ecdsa-nistp256'
    	 | 'ssh-ecdsa-nistp384'
             | 'ssh-ecdsa-nistp521'
             | 'ssh-ed25519'
    	 | 'ssh-ed448'
    base64-encoded-key :: % The user's public key
    comment :: % Comments are skipped
    ```

  - `known_hosts`{: #FILE-known_hosts } \- host keys from hosts visited
    concatenated. The file is created and used by the client.

    It is composed of lines as for
    [OpenSSH](https://man.openbsd.org/sshd#SSH_KNOWN_HOSTS_FILE_FORMAT):

    ```text
    (option)? pattern(,pattern)* keytype key (comment)?
    ```

    where

    ```text
    option :: '@revoked'
    pattern :: host | '[' host ']:' port
    host :: ip-address | hostname | '*'
    port :: portnumber | '*'
    keytype :: 'ssh-dsa'
             | 'ssh-rsa'
             | 'ssh-ecdsa-nistp256'
    	 | 'ssh-ecdsa-nistp384'
             | 'ssh-ecdsa-nistp521'
             | 'ssh-ed25519'
    	 | 'ssh-ed448'
    key :: % encoded key from eg ssh_host_*.pub
    ```

  - [](){: #FILE-id_STAR } `id_dsa`{: #FILE-id_dsa } \- private dss user key
    (optional)
  - `id_rsa`{: #FILE-id_rsa } \- private rsa user key (optional)
  - `id_ecdsa`{: #FILE-id_ecdsa } \- private ecdsa user key (optional)
  - `id_ed25519`{: #FILE-id_ed25519 } \- private eddsa user key for curve 25519
    (optional)
  - `id_ed448`{: #FILE-id_ed448 } \- private eddsa user key for curve 448
    (optional)

  The key files could be generated with OpenSSH's ssh-keygen command.

  The default value of USERDIR is
  `/home/`[`LOCALUSER`](`m:ssh_file#LOCALUSER`)`/.ssh`.

  To change the USERDIR, see the [user_dir](`t:user_dir_common_option/0`) option
""".
-moduledoc(#{since => "OTP 21.2"}).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

-include("ssh.hrl").

%% experimental:
-export([decode_ssh_file/4
        ]).

%%%--------------------- server exports ---------------------------
-behaviour(ssh_server_key_api).
-export([host_key/2, is_auth_key/3]).
-export_type([system_dir_daemon_option/0]).
-doc "Sets the [system directory](`m:ssh_file#SYSDIR`).".
-doc(#{group => <<"Options">>}).
-type system_dir_daemon_option()   :: {system_dir, string()}.

%%%--------------------- client exports ---------------------------
-behaviour(ssh_client_key_api).
-export([is_host_key/5, user_key/2, add_host_key/4]).
-export_type([pubkey_passphrase_client_options/0]).
-doc """
If the user's DSA, RSA or ECDSA key is protected by a passphrase, it can be
supplied with thoose options.

Note that EdDSA passhrases (Curves 25519 and 448) are not implemented.
""".
-doc(#{group => <<"Options">>}).
-type pubkey_passphrase_client_options() ::   {dsa_pass_phrase,      string()}
                                            | {rsa_pass_phrase,      string()}
                                              %% Not yet implemented:                     | {ed25519_pass_phrase,  string()}
                                              %% Not yet implemented:                     | {ed448_pass_phrase,    string()}
                                            | {ecdsa_pass_phrase,    string()} .

%%%--------------------- utility exports ---------------------------
-export([decode/2, encode/2]).
-export([extract_public_key/1]).

-define(ENCODED_LINE_LENGTH, 68).

%%%--------------------- common exports ---------------------------
-export_type([user_dir_common_option/0,
              user_dir_fun_common_option/0
             ]).

-doc "Sets the [user directory](`m:ssh_file#USERDIR`).".
-doc(#{group => <<"Options">>}).
-type user_dir_common_option()     :: {user_dir,  string()}.
-doc(#{group => <<"Options">>}).
-type user_dir_fun_common_option() :: {user_dir_fun, user2dir()}.
-doc """
Sets the [user directory](`m:ssh_file#USERDIR`) dynamically by evaluating the
`user2dir` function.
""".
-doc(#{group => <<"Options">>}).
-type user2dir() :: fun((RemoteUserName::string()) -> UserDir :: string()) .

-doc """
Make the handling of large files fast by setting `time`, but this will use more
memory. The `space` variant shrinks the memory requirements, but with a higher
time consumption.

To set it, set the option `{key_cb, {ssh_file, [{optimize,TimeOrSpace}]}` in the
call of ["ssh:connect/3](`ssh:connect/3`), `ssh:daemon/2` or similar function
call that initiates an ssh connection.
""".
-doc(#{group => <<"Options">>}).
-type optimize_key_lookup() :: {optimize, time|space} .

-doc "The key representation".
-doc(#{group => <<"Options">>}).
-type key() :: public_key:public_key() | public_key:private_key() .
-doc(#{group => <<"Options">>}).
-type experimental_openssh_key_v1() :: [{key(), openssh_key_v1_attributes()}].
-doc "Types for the experimental implementaition of the `openssh_key_v1` format.".
-doc(#{group => <<"Options">>}).
-type openssh_key_v1_attributes() :: [{atom(),term()}].

%%%================================================================
%%%
%%% API
%%%

%%%---------------- SERVER API ------------------------------------
-doc """
**Types and description**

See the api description in
[ssh_server_key_api, Module:host_key/2](`c:ssh_server_key_api:host_key/2`).

**Options**

- [system_dir](`t:system_dir_daemon_option/0`)

**Files**

- [`SYSDIR/ssh_host_rsa_key`](`m:ssh_file#FILE-ssh_host_rsa_key`)
- [`SYSDIR/ssh_host_dsa_key`](`m:ssh_file#FILE-ssh_host_dsa_key`)
- [`SYSDIR/ssh_host_ecdsa_key`](`m:ssh_file#FILE-ssh_host_ecdsa_key`)
- [`SYSDIR/ssh_host_ed25519_key`](`m:ssh_file#FILE-ssh_host_ed25519_key`)
- [`SYSDIR/ssh_host_ed448_key`](`m:ssh_file#FILE-ssh_host_ed448_key`)
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec host_key(Algorithm, Options) -> Result when
      Algorithm :: ssh:pubkey_alg(),
      Result :: {ok, public_key:private_key()} | {error, term()},
      Options :: ssh_server_key_api:daemon_key_cb_options(none()).

host_key(Algorithm, Opts) ->
    read_ssh_key_file(system, private, Algorithm, Opts).

%%%................................................................
-doc """
**Types and description**

See the api description in
[ssh_server_key_api: Module:is_auth_key/3](`c:ssh_server_key_api:is_auth_key/3`).

**Options**

- [user_dir_fun](`t:user_dir_fun_common_option/0`)
- [user_dir](`t:user_dir_common_option/0`)

**Files**

- [`USERDIR/authorized_keys`](`m:ssh_file#FILE-authorized_keys`)
- [`USERDIR/authorized_keys2`](`m:ssh_file#FILE-authorized_keys2`)

This functions discards all options in the beginning of the lines of thoose
files when reading them.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec is_auth_key(Key, User, Options) -> boolean() when
      Key :: public_key:public_key(),
      User :: string(),
      Options :: ssh_server_key_api:daemon_key_cb_options(optimize_key_lookup()).

is_auth_key(Key0, User, Opts) ->
    Dir = ssh_dir({remoteuser,User}, Opts),
    ok = assure_file_mode(Dir, user_read),
    KeyType = normalize_alg(
                erlang:atom_to_binary(ssh_transport:public_algo(Key0), latin1)),
    Key = encode_key(Key0),
    lookup_auth_keys(KeyType, Key, filename:join(Dir,"authorized_keys"), Opts)
        orelse
        lookup_auth_keys(KeyType, Key, filename:join(Dir,"authorized_keys2"), Opts).

%%%---------------- CLIENT API ------------------------------------
-doc """
**Types and description**

See the api description in
[ssh_client_key_api, Module:user_key/2](`c:ssh_client_key_api:user_key/2`).

**Options**

- [user_dir](`t:user_dir_common_option/0`)
- [dsa_pass_phrase](`t:pubkey_passphrase_client_options/0`)
- [rsa_pass_phrase](`t:pubkey_passphrase_client_options/0`)
- [ecdsa_pass_phrase](`t:pubkey_passphrase_client_options/0`)

Note that EdDSA passhrases (Curves 25519 and 448) are not implemented.

**Files**

- [`USERDIR/id_dsa`](`m:ssh_file#FILE-id_dsa`)
- [`USERDIR/id_rsa`](`m:ssh_file#FILE-id_rsa`)
- [`USERDIR/id_ecdsa`](`m:ssh_file#FILE-id_ecdsa`)
- [`USERDIR/id_ed25519`](`m:ssh_file#FILE-id_ed25519`)
- [`USERDIR/id_ed448`](`m:ssh_file#FILE-id_ed448`)
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec user_key(Algorithm, Options) -> Result when
      Algorithm :: ssh:pubkey_alg(),
      Result :: {ok, public_key:private_key()} |
                {error, string()},
      Options :: ssh_client_key_api:client_key_cb_options(none()).

user_key(Algorithm, Opts) ->
    read_ssh_key_file(user, private, Algorithm, Opts).

%%%................................................................
%%% New style (with port number)
-doc """
**Types and description**

See the api description in
[ssh_client_key_api, Module:is_host_key/5](`c:ssh_client_key_api:is_host_key/5`).

[](){: #is_host_key-4 }

Note that the alternative, the old
[Module:is_host_key/4](`c:ssh_client_key_api:is_host_key/4`) is no longer
supported by `ssh_file`.

**Option**

- [user_dir](`t:user_dir_common_option/0`)

**File**

- [`USERDIR/known_hosts`](`m:ssh_file#FILE-known_hosts`)
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec is_host_key(Key, Host, Port, Algorithm, Options) -> Result when
      Key :: public_key:public_key(),
      Host :: inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
      Port :: inet:port_number(),
      Algorithm :: ssh:pubkey_alg(),
      Options :: ssh_client_key_api:client_key_cb_options(optimize_key_lookup()),
      Result :: boolean() | {error, term()} .

is_host_key(Key0, Hosts0, Port, Algorithm, Opts) ->
    Dir = ssh_dir(user, Opts),
    File = filename:join(Dir, "known_hosts"),
    Hosts = [list_to_binary(H) || H <- normalize_hosts_list(Hosts0, Port)],
    KeyType = normalize_alg(erlang:atom_to_binary(Algorithm, latin1)),
    Key = encode_key(Key0),
    ok = assure_file_mode(File, user_read),
    lookup_host_keys(Hosts, KeyType, Key, File, Opts).

%%%----------------------------------------------------------------
-doc """
**Types and description**

See the api description in
[ssh_client_key_api, Module:add_host_key/4](`c:ssh_client_key_api:add_host_key/4`).

[](){: #add_host_key-3 }

Note that the alternative, the old
[Module:add_host_key/3](`c:ssh_client_key_api:add_host_key/3`) is no longer
supported by `ssh_file`.

**Option**

- [user_dir](`t:user_dir_common_option/0`)

**File**

- [`USERDIR/known_hosts`](`m:ssh_file#FILE-known_hosts`)
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec add_host_key(Host, Port, Key, Options) -> Result when 
      Host :: inet:ip_address() | inet:hostname()
            | [inet:ip_address() | inet:hostname()],
      Port :: inet:port_number(),
      Key :: public_key:public_key(),
      Options :: ssh_client_key_api:client_key_cb_options(none()),
      Result :: ok | {error, term()}.

add_host_key(Hosts0, Port, Key, Opts) ->
    File = file_name(user, "known_hosts", Opts),
    assure_file_mode(File, user_write),
    case file:open(File, [write,append]) of
	{ok, Fd} ->
            KeyType = erlang:atom_to_binary(ssh_transport:public_algo(Key), latin1),
            EncKey = ssh_message:ssh2_pubkey_encode(Key),
            Hosts1 = normalize_hosts_list(Hosts0, Port),
            SshBin =
                iolist_to_binary([lists:join(",", Hosts1), " ",
                                  KeyType," ",base64:encode(iolist_to_binary(EncKey)),
                                  "\n"]),
            Res = file:write(Fd, SshBin),
	    file:close(Fd),
	    Res;
	{error,Error} ->
	    {error,{add_host_key,Error}}
    end.

%%%---------------- UTILITY API -----------------------------------
%%% In public key before OTP-24.0 as ssh_decode/2 and ssh_encode/2

-doc """
Decodes an SSH file-binary.

If `Type` is `public_key` the binary can be either an RFC4716 public key or an
OpenSSH public key.

> #### Note {: .info }
>
> The implementation of the `openssh_key_v1` format is still experimental.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec decode(SshBin, Type) -> Decoded | {error,term()}
                                  when SshBin :: binary(),
                                       Type :: ssh2_pubkey
                                             | public_key
                                             | openssh_key
                                             | rfc4716_key
                                             | openssh_key_v1  % Experimental
                                             | known_hosts
                                             | auth_keys,
                                       Decoded :: Decoded_ssh2_pubkey
                                                | Decoded_public
                                                | Decoded_openssh
                                                | Decoded_rfc4716
                                                | Decoded_openssh_key_v1
                                                | Decoded_known_hosts
                                                | Decoded_auth_keys,

                                       Decoded_ssh2_pubkey :: public_key:public_key(),
                                       Decoded_public :: Decoded_rfc4716
                                                       | Decoded_openssh_key_v1
                                                       | Decoded_openssh,
                                       Decoded_openssh :: [{public_key:public_key(), [{comment,string()}]}],
                                       Decoded_rfc4716 :: [{key(), [{headers,Attrs}]}],
                                       Decoded_openssh_key_v1 :: experimental_openssh_key_v1(),
                                       Decoded_known_hosts :: [{public_key:public_key(), [{comment,string()}
                                                                                          | {hostnames,[string()]}]}],
                                       Decoded_auth_keys :: [{public_key:public_key(), [{comment,string()}
                                                                                        | {options,[string()]}]}],
                                       Attrs :: {Key::string(), Value::string()} .

decode(KeyBin, ssh2_pubkey) when is_binary(KeyBin) ->
    ssh_message:ssh2_pubkey_decode(KeyBin);

decode(KeyBin, public_key) when is_binary(KeyBin) ->
    Type = case KeyBin of
               <<"-----BEGIN OPENSSH",_/binary>> -> openssh_key_v1;
               <<"----",_/binary>> -> rfc4716_key;
               _ -> openssh_key
           end,
    decode(KeyBin, Type);

decode(KeyBin, Type) when is_binary(KeyBin) andalso 
                          (Type==rfc4716_key orelse
                           Type==openssh_key_v1 % Experimental
                          ) ->
    %% Ex: <<"---- BEGIN SSH2 PUBLIC KEY ----\n....">>     (rfc4716_key)
    %%     <<"-----BEGIN OPENSSH PRIVATE KEY-----\n....">> (openssh_key_v1)
    case decode_ssh_file(public, any, KeyBin, ignore) of
        {ok,Keys} ->
            [{Key,
              if
                  Attrs =/= [] ->
                      [{headers, [{binary_to_list(K),binary_to_list(V)} || {K,V} <- Attrs]}];
                  Attrs == [] ->
                      []
              end
             }
             || {Key,Attrs} <- Keys];

        {error,Error} ->
            {error,Error}
    end;

decode(KeyBin0, openssh_key) when is_binary(KeyBin0) ->
    %% Ex: <<"ssh-rsa AAAAB12....3BC someone@example.com">>
    try
        [begin
             [_,K|Rest] = binary:split(Line, <<" ">>, [global,trim_all]),
             Key = ssh_message:ssh2_pubkey_decode(base64:decode(K)),
             case Rest of
                 [Comment] -> {Key, [{comment,binary_to_list(Comment)}]};
                 [] -> {Key,[]}
             end
         end || Line <- split_in_nonempty_lines(KeyBin0)
        ]
    catch
        _:_ -> {error, key_decode_failed}
    end;

decode(Bin, known_hosts) when is_binary(Bin) ->
    [begin
         Attrs = 
             [
              {comment, binary_to_list(erlang:iolist_to_binary(lists:join(" ", Comment)))}
              || Comment =/= []
             ] ++
             [
              {hostnames,
               [binary_to_list(HP)
                || HP <- binary:split(HostPort,<<",">>,[global,trim_all])
               ]}
             ],
         {ssh_message:ssh2_pubkey_decode(base64:decode(KeyBin)),
          Attrs
         }
     end
     || L <- split_in_nonempty_lines(Bin),
        [HostPort,_KeyType,KeyBin|Comment] <- [binary:split(L,<<" ">>,[global,trim_all])]
    ];

decode(Bin, auth_keys) when is_binary(Bin) ->
    [begin
         Attrs = 
             [
              {comment, binary_to_list(erlang:iolist_to_binary(lists:join(" ", Comment)))}
              || Comment =/= []
             ] ++
             [
              {options, lists:map(fun erlang:binary_to_list/1, Options)}
              || Options =/= []
             ],
         {ssh_message:ssh2_pubkey_decode(base64:decode(KeyBin)),
          Attrs
         }
     end
     || L <- split_in_nonempty_lines(Bin),
        [Options,_KeyType,KeyBin|Comment] <-
            case binary:match(L, [<<"ssh-rsa">>,
                                  <<"rsa-sha2-">>,
                                  <<"ssh-dss">>,
                                  <<"ecdsa-sha2-nistp">>,
                                  <<"ssh-ed">>
                                 ]) of
                nomatch ->
                    [];
                {0, Len} when is_integer(Len) ->
                    [ [[] | binary:split(L,<<" ">>,[global,trim_all])] ];
                {Pos,Len} when is_integer(Pos), is_integer(Len) ->
                    [ [binary:split(binary:part(L,0,Pos-1), <<",">>,[global,trim_all]) |
                       binary:split(binary:part(L,Pos,byte_size(L)-Pos), <<" ">>, [global,trim_all])]
                    ]
            end
    ];

decode(_KeyBin, _Type) ->
    error(badarg).

%%%----------------------------------------------------------------
-doc """
Encodes a list of SSH file entries (public keys and attributes) to a binary.

> #### Note {: .info }
>
> The implementation of the `openssh_key_v1` format is still experimental.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec encode(InData, Type) -> binary() | {error,term()}
                                  when Type :: ssh2_pubkey
                                             | openssh_key
                                             | rfc4716_key
                                             | openssh_key_v1  % Experimental
                                             | known_hosts
                                             | auth_keys,
                                       InData :: InData_ssh2_pubkey
                                               | InData_openssh
                                               | InData_rfc4716
                                               | InData_openssh_key_v1
                                               | InData_known_hosts
                                               | InData_auth_keys,

                                       InData_ssh2_pubkey :: public_key:public_key(),
                                       InData_openssh :: [{public_key:public_key(), [{comment,string()}]}],
                                       InData_rfc4716 :: [{key(), [{headers,Attrs}]}],
                                       InData_openssh_key_v1 :: experimental_openssh_key_v1(),
                                       InData_known_hosts :: [{public_key:public_key(), [{comment,string()}
                                                                                          | {hostnames,[string()]}]}],
                                       InData_auth_keys :: [{public_key:public_key(), [{comment,string()}
                                                                                        | {options,[string()]}]}],
                                       Attrs :: {Key::string(), Value::string()} .

encode(Key, ssh2_pubkey) ->
    ssh_message:ssh2_pubkey_encode(Key);

encode(KeyAttrs, Type) when Type==rfc4716_key ;
                            Type==openssh_key_v1 % Experimental
                            ->
    {Begin, End, F} =
        case Type of
            rfc4716_key ->
                {"---- BEGIN SSH2 PUBLIC KEY ----\n",
                 "---- END SSH2 PUBLIC KEY ----\n",
                 fun ssh_message:ssh2_pubkey_encode/1};
            openssh_key_v1 ->
                {"-----BEGIN OPENSSH PRIVATE KEY-----\n",
                 "-----END OPENSSH PRIVATE KEY-----\n",
                 fun openssh_key_v1_encode/1}
        end,
    iolist_to_binary(
      [
       [Begin,
        [rfc4716_encode_header(H) || H <- proplists:get_value(headers, Attrs, [])],
        split_long_lines( base64:encode( F(Key) ) ),
        "\n",
        End
       ] ||
          {Key,Attrs} <- KeyAttrs
      ]
     );

encode(KeyAttrs, Type) when Type == known_hosts;
                            Type == auth_keys ;
                            Type == openssh_key ->
    FirstArgTag =
        case Type of
            known_hosts -> hostnames;
            auth_keys -> options;
            openssh_key -> '*no tag*'
        end,
    iolist_to_binary(
      [
       begin
           <<?DEC_BIN(KeyType,__0),_/binary>> = Enc = ssh_message:ssh2_pubkey_encode(Key),
           [case lists:join(",", proplists:get_value(FirstArgTag, Attributes, [])) of
                [] -> "";
                C -> [C," "]
            end,
            KeyType, " ",
            base64:encode(Enc), " ",
            case proplists:get_value(comment, Attributes, []) of
                [] -> "";
                C -> C
            end,
            "\n"
           ]
       end
       || {Key,Attributes} <- KeyAttrs
      ]
     );

encode(_KeyBin, _Type) ->
    error(badarg).

%%%----------------------------------------------------------------

-doc "Fetches the public key from a private key.".
-doc(#{since => <<"OTP 25.0">>}).
-spec extract_public_key(PrivKey) -> PubKey
                        when PrivKey :: public_key:private_key(),
                              PubKey :: public_key:public_key().

extract_public_key(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #'RSAPublicKey'{modulus = N, publicExponent = E};
extract_public_key(#'DSAPrivateKey'{y = Y, p = P, q = Q, g = G}) ->
    {Y,  #'Dss-Parms'{p=P, q=Q, g=G}};
extract_public_key(#'ECPrivateKey'{parameters = {namedCurve,OID},
				   publicKey = Pub0, privateKey = Priv}) when
      OID == ?'id-Ed25519' orelse
      OID == ?'id-Ed448' ->
    case {pubkey_cert_records:namedCurves(OID), Pub0} of
        {Alg, asn1_NOVALUE} ->
            %% If we're missing the public key, we can create it with
            %% the private key.
            {Pub, Priv} = crypto:generate_key(eddsa, Alg, Priv),
            {#'ECPoint'{point=Pub}, {namedCurve,OID}};
        {_Alg, Pub} ->
            {#'ECPoint'{point=Pub}, {namedCurve,OID}}
    end;
extract_public_key(#'ECPrivateKey'{parameters = {namedCurve,OID},
				   publicKey = Q}) when is_tuple(OID) ->
    {#'ECPoint'{point=Q}, {namedCurve,OID}};
extract_public_key(#{engine:=_, key_id:=_, algorithm:=Alg} = M) ->
    case {Alg, crypto:privkey_to_pubkey(Alg, M)} of
        {rsa, [E,N]} ->
            #'RSAPublicKey'{modulus = N, publicExponent = E};
        {dss, [P,Q,G,Y]} ->
            {Y, #'Dss-Parms'{p=P, q=Q, g=G}}
    end.

%%%================================================================
%%%
%%% Local functions
%%%

%%%---------------- SERVER FUNCTIONS ------------------------------

lookup_auth_keys(KeyType, Key, File, Opts) ->
    case get_kb_option(optimize, Opts, time) of
        time ->
            case file:read_file(File) of
                {ok,Bin} ->
                    Lines = split_in_lines(Bin),
                    find_key(KeyType, Key, Lines);
                _ ->
                    false
            end;
        space ->
            case file:open(File, [read, binary]) of
                {ok, Fd} ->
                    Result =
                        read_test_loop(Fd,
                                       fun(Line) ->
                                               find_key(KeyType, Key, [Line])
                                       end),
                    file:close(Fd),
                    Result;
                {error,_Error} ->
                   false
            end;
        Other ->
            {error,{is_auth_key,{opt,Other}}}
    end.


find_key(KeyType, Key, [<<"#",_/binary>> | Lines]) ->
    find_key(KeyType, Key, Lines);
find_key(KeyType, Key, [Line | Lines]) ->
    try
        [E1,E2|Es] = binary:split(Line, <<" ">>, [global,trim_all]),
        [normalize_alg(E1), normalize_alg(E2) | Es] % KeyType is in first or second element
    of
        [_Options, KeyType, Key | _Comment] ->
            true;
        [KeyType, Key | _Comment] ->
            true;
        _ ->
            find_key(KeyType, Key, Lines)
    catch
        _:_ ->
            find_key(KeyType, Key, Lines)
    end;
find_key(_, _, _) ->
    false.


%%%---------------- CLIENT FUNCTIONS ------------------------------

normalize_alg(<<"rsa-sha2-",_/binary>>) -> <<"ssh-rsa">>;
normalize_alg(X) -> X.

%%%--------------------------------
normalize_hosts_list(Hosts, Port) when is_list(hd(Hosts)) ->
    lists:reverse(
      lists:foldl(fun(H0, Acc) ->
                          H1s = add_ip(replace_localhost(H0)),
                          Hs = case Port of
                                   22 -> H1s;
                                   _ -> [lists:concat(["[",Hx,"]:",Port]) || Hx <- H1s]
                               end,
                          lists:foldl(
                            fun(Hy, Acc2) ->
                                    case lists:member(Hy, Acc2) of
                                        true ->
                                            Acc2;
                                        false ->
                                            [Hy|Acc2]
                                    end
                            end, Acc, Hs)
                  end, [], Hosts));
normalize_hosts_list(Hosts, Port) ->
    normalize_hosts_list([Hosts], Port).

replace_localhost(any) ->
    replace_localhost("localhost");
replace_localhost(loopback) ->
    replace_localhost("localhost");
replace_localhost("localhost") ->
    {ok, Hostname} = inet:gethostname(),
    Hostname;
replace_localhost(H) when is_atom(H) ->
    replace_localhost(atom_to_list(H));
replace_localhost(Host) ->
    Host.

add_ip(IP) when is_tuple(IP) ->
    [ssh_connection:encode_ip(IP)];
add_ip(Host) ->
    case inet:getaddr(Host, inet) of
	{ok, Addr} ->
	    case ssh_connection:encode_ip(Addr) of
		false -> [Host];
                Host -> [Host];
		IPString -> [Host,IPString]
	    end;
	_ -> [Host]
    end.

%%%--------------------------------
encode_key(Key) ->
    base64:encode(
      iolist_to_binary(
        ssh_message:ssh2_pubkey_encode(Key))).

%%%--------------------------------
read_test_loop(Fd, Test) ->
    case io:get_line(Fd, '') of
	eof ->
            file:close(Fd),
	    false;
	{error,Error} ->
	    %% Rare... For example NFS errors
	    {error,Error};
	Line0 ->
            case split_in_lines(Line0) of % remove trailing EOL
                [Line] ->
                    case Test(Line) of
                        false ->
                            read_test_loop(Fd, Test);
                        Other ->
                            Other
                    end;
                _ ->
                    read_test_loop(Fd, Test)
            end
    end.

%%%--------------------------------

lookup_host_keys(Hosts, KeyType, Key, File, Opts) ->
    case get_kb_option(optimize, Opts, time) of
        time ->
            case file:read_file(File) of
                {ok,Bin} ->
                    Lines = split_in_lines(Bin),
                    case find_host_key(Hosts, KeyType, Key, Lines) of
                        {true,RestLines} ->
                            case revoked_key(Hosts, KeyType, Key, RestLines) of
                                true ->
                                    {error,revoked_key};
                                false ->
                                    true
                            end;
                        false ->
                            false
                    end;
                {error,enoent} ->
                    false;
                {error,Error} ->
                    {error,{is_host_key,Error}}
            end;
        space ->
            case file:open(File, [read, binary]) of
                {ok, Fd} ->
                    Result =
                        case read_test_loop(Fd,
                                            fun(Line) ->
                                                    find_host_key(Hosts, KeyType, Key, [Line])
                                            end)
                        of
                            {true,_} ->
                                %% The key is found, now check the rest of the file to see if it is
                                %% revoked
                                case read_test_loop(Fd,
                                                    fun(Line) ->
                                                            revoked_key(Hosts, KeyType, Key, [Line])
                                                    end)
                                of
                                    true ->
                                        {error,revoked_key};
                                    false ->
                                        true
                                end;
                            {error,Error} ->
                                {error,{is_host_key,Error}};
                            Other ->
                                Other
                        end,
                    file:close(Fd),
                    Result;
                {error,Error} ->
                    {error,Error}
            end;
        Other ->
            {error,{is_host_key,{opt,Other}}}
    end.


find_host_key(Hosts, KeyType, EncKey, [<<"#",_/binary>>|PatternLines]) ->
    %% skip comments
    find_host_key(Hosts, KeyType, EncKey, PatternLines);
find_host_key(Hosts, KeyType, EncKey, [Line|PatternLines]) ->
    %% split the line into the separate parts:
    %%    option? pattern(,pattern)* keytype key comment?
    SplitLine = binary:split(Line, <<" ">>, [global,trim_all]),
    case known_key_in_line(Hosts, KeyType, EncKey, SplitLine) of
        true ->
            {true, PatternLines};
        false ->
            find_host_key(Hosts, KeyType, EncKey, PatternLines)
    end;
find_host_key(_, _, _, []) ->
    false.


revoked_key(Hosts, KeyType, EncKey, [<<"@revoked ",RestLine/binary>> | Lines]) ->
    case binary:split(RestLine, <<" ">>, [global,trim_all]) of
        [Patterns, KeyType, EncKey|_Comment] ->
            %% Very likely to be a revoked key,
            %% but does any of the hosts match the pattern?
            case host_match(Hosts, Patterns) of
                true ->
                    true;
                false ->
                    revoked_key(Hosts, KeyType, EncKey, Lines)
            end;
        _ ->
            revoked_key(Hosts, KeyType, EncKey, Lines)
    end;
revoked_key(Hosts, KeyType, EncKey, [_ | Lines]) ->
    %% Not a revokation line, check the rest
    revoked_key(Hosts, KeyType, EncKey, Lines);
revoked_key(_, _, _, _) ->
    false.


known_key_in_line(Hosts, KeyType, EncKey, FullLine=[Option | Rest]) ->
    case line_match(Hosts, KeyType, EncKey, Rest) of
        true ->
            case Option of
                <<"@revoked">> ->
                    {error, revoked_key};
                _ ->
                    %% No other options than @revoked handled (but the key matched)
                    false
            end;
        false ->
            line_match(Hosts, KeyType, EncKey, FullLine)
    end;
known_key_in_line(_, _, _, _) ->
    false.


line_match(Hosts, KeyType, EncKey, [Patterns, KeyType0, EncKey0|_Comment]) ->
    KeyType==normalize_alg(KeyType0)
        andalso EncKey==EncKey0
        andalso host_match(Hosts, Patterns);
line_match(_, _, _, _) ->
    false.



host_match(Hosts, Patterns) ->
    PatternList = binary:split(Patterns, <<",">>, [global]),
    host_matchL(Hosts, PatternList).

host_matchL([H|Hosts], Patterns) ->
    case one_host_match(H, Patterns) of
        true ->
            true;
        false ->
            host_matchL(Hosts, Patterns)
    end;
host_matchL(_, _) ->
    false.


one_host_match(H, [Pat|Patterns]) ->
    case pos_match(H, Pat) of
        true ->
            %% Not true if there is any "!" pattern that matches
            not lists:any(fun(P) -> neg_match(H,P) end,
                          Patterns);
        false ->
            one_host_match(H, Patterns)
    end;
one_host_match(_, _) ->
    false.


neg_match(H, <<"!",P/binary>>) ->
    pos_match(H, P);
neg_match(_, _) ->
    false.


pos_match(_, <<"*">>    ) -> true;
pos_match(_, <<"*:*">>  ) -> true;
pos_match(_, <<"[*]:*">>) -> true;
pos_match(H, <<"!",P/binary>>) -> not pos_match(H, P);
pos_match(H, H) -> true;
pos_match(H, P) ->
    case
        {binary:split(H,<<":">>),
         binary:split(P, <<":">>)}
    of
        {[Hh,_], [Ph,<<"*">>]} ->
            %% [host]:port [host]:*
            Ph == Hh;

        {[Hh], [Ph,<<"*">>]} ->
            %% host [host]:*
            Sz = byte_size(Hh),
            Ph == <<"[", Hh:Sz/binary, "]">>;

        {[Hh], [Ph,<<"22">>]} ->
            %% host [host]:22
            Sz = byte_size(Hh),
            Ph == <<"[", Hh:Sz/binary, "]">>;

        _ ->
            false
    end.

%%%---------------- UTILITY ---------------------------------------
rfc4716_encode_header({Tag, Value}) ->
    TagLen = length(Tag),
    ValueLen = length(Value),
    case TagLen + 1 + ValueLen of
	N when N > ?ENCODED_LINE_LENGTH ->
	    NumOfChars =  ?ENCODED_LINE_LENGTH - (TagLen + 1),
	    {First, Rest} = lists:split(NumOfChars, Value),
	    [Tag,": " , First, [$\\], "\n", rfc4716_encode_value(Rest) , "\n"];
	_ ->
	    [Tag, ": ", Value, "\n"]
    end.

rfc4716_encode_value(Value) ->
    case length(Value) of
	N when N > ?ENCODED_LINE_LENGTH ->
	    {First, Rest} = lists:split(?ENCODED_LINE_LENGTH, Value),
	    [First, [$\\], "\n", rfc4716_encode_value(Rest)];
	_ ->
	    Value
    end.

split_long_lines(<<Text:?ENCODED_LINE_LENGTH/binary, Rest/binary>>) when Rest =/= <<"">> ->
    [Text, $\n | split_long_lines(Rest)];
split_long_lines(Bin) ->
    [Bin].

%%%---------------- COMMON FUNCTIONS ------------------------------

assure_file_mode(File, user_write) -> assure_file_mode(File, 8#200);
assure_file_mode(File, user_read) -> assure_file_mode(File, 8#400);
assure_file_mode(File, Mode) ->
    case file:read_file_info(File) of
        {ok,#file_info{mode=FileMode}} ->
            case (FileMode band Mode) of % is the wanted Mode set?
                Mode -> 
                    %% yes
                    ok;
                _ ->
                    %% no
                    file:change_mode(File, FileMode bor Mode) % set missing bit(s)
            end;
        {error,enoent} ->
            %% Not yet created
            ok;
        {error,Error} ->
            {error,Error}
    end.


get_kb_option(Key, Opts, Default) ->
    try
        proplists:get_value(Key, 
                            proplists:get_value(key_cb_private, Opts, []),
                            Default)
    catch
        _:_ ->
            Default
    end.


read_ssh_key_file(Role, PrivPub, Algorithm, Opts) ->
    File = file_name(Role, file_base_name(Role,Algorithm), Opts),
    Password = %% Pwd for Host Keys is an undocumented option and should not be used
        proplists:get_value(identity_pass_phrase(Algorithm), Opts, ignore),

    ok = assure_file_mode(File, user_read),
    case file:read_file(File) of
        {ok, Pem} ->
            try
                decode_ssh_file(PrivPub, Algorithm, Pem, Password)
            of
                {ok, [{Key,_Attrs}|_Keys]} ->
                    {ok,Key};
                {error, Reason} ->
                    {error, Reason}
            catch
                throw:Reason ->
                    {error, Reason};
                error:Reason ->
                    {error, Reason}
            end;

        {error, Reason} ->
            {error, Reason}
    end.


-doc false.
-spec decode_ssh_file(PrivPub, Algorithm, Pem, Password) -> Result when
      PrivPub :: private | public,
      Algorithm :: ssh:pubkey_alg() | any,
      Pem :: binary(),
      Password :: string() | ignore,
      Result :: {ok, Keys} | {error, any()},
      Keys :: [{Key,Attrs}],
      Attrs :: [{any(),any()}],
      Key :: public_key:private_key() | public_key:public_key() .

decode_ssh_file(PrivPub, Algorithm, Pem, Password) ->
    try decode_pem_keys(Pem, Password)
    of
        {ok, Keys} when Algorithm == any ->
            {ok, Keys};

        {ok, Keys0} ->
            case [{Key,Attrs} || {Key,Attrs} <- Keys0,
                                 ssh_transport:valid_key_sha_alg(PrivPub, Key, Algorithm)] of
                [] ->
                    {error,no_key_found};
                Keys ->
                    {ok,Keys}
            end;

        {error,Error} ->
            {error,Error}

    catch
        _:_ ->
            {error, key_decode_failed}
    end.


decode_pem_keys(RawBin, Password) ->
    PemLines = split_in_lines(
                 binary:replace(RawBin, [<<"\\\n">>,<<"\\\r\\\n">>],  <<"">>, [global])
                ),
    decode_pem_keys(PemLines, Password, []).
decode_pem_keys([], _, Acc) ->
    {ok,lists:reverse(Acc)};


decode_pem_keys(PemLines, Password, Acc) ->
    %% Private Key
    try get_key_part(PemLines) of
        {'openssh-key-v1', Bin, Attrs, RestLines} ->
            %% -----BEGIN OPENSSH PRIVATE KEY-----
            %% Holds both public and private keys
            KeyPairs = openssh_key_v1_decode(Bin, Password),
            Keys = [{Key,Attrs} || {Pub,Priv} <- KeyPairs,
                                   Key <- [Pub,Priv]],
            decode_pem_keys(RestLines, Password, Keys ++ Acc);

        {rfc4716, Bin, Attrs, RestLines} ->
            %% ---- BEGIN SSH2 PUBLIC KEY ----
            %% rfc4716 only defines public keys
            Key = ssh_message:ssh2_pubkey_decode(Bin),
            decode_pem_keys(RestLines, Password, [{Key,Attrs}|Acc]);

        {Type, Bin, Attrs, RestLines} ->
            %% -----BEGIN (RSA|DSA|EC) PRIVATE KEY-----
            %% and possibly others
            case get_encrypt_hdrs(Attrs) of
                not_encrypted ->
                    Key = public_key:pem_entry_decode({Type,Bin,not_encrypted}),
                    decode_pem_keys(RestLines, Password, [{Key,Attrs}|Acc]);

                [Cipher,Salt] when is_binary(Cipher),
                                   is_binary(Salt),
                                   Password =/= ignore ->
                    CryptInfo =
                        {binary_to_list(Cipher), unhex(binary_to_list(Salt))},
                    Key = public_key:pem_entry_decode({Type,Bin,CryptInfo}, Password),
                    decode_pem_keys(RestLines, Password, [{Key,Attrs}|Acc]);

                _X ->
                    {error, no_pass_phrase}
            end
    catch
        _:_ -> error(bad_or_unsupported_key_format)
    end.

get_encrypt_hdrs(KVs) ->
    lists:foldl(fun({<<"Proc-Type">>, <<"4,ENCRYPTED", _/binary>>}, _Acc) ->
                        {proc_type, <<"4,ENCRYPTED">>};
                   ({<<"DEK-Info">>, DEKinfo}, {proc_type,_}) ->
                        binary:split(DEKinfo, <<",">>);
                   (_, Acc) ->
                        Acc
                end, not_encrypted, KVs).

unhex(S) ->
    %% I would like to do erlang:list_to_integer(S,16), but that does not fit
    %% the public_key:pem_entry_decode API
    list_to_binary(
      lists:foldr(fun(D2, {D1,Acc}) ->
                          [erlang:list_to_integer([D2,D1], 16) | Acc]; % sic!
                     (D1, Acc) when is_list(Acc) ->
                          {D1,Acc}
                  end, [], S)).

file_base_name(user,   'ecdsa-sha2-nistp256') -> "id_ecdsa";
file_base_name(user,   'ecdsa-sha2-nistp384') -> "id_ecdsa";
file_base_name(user,   'ecdsa-sha2-nistp521') -> "id_ecdsa";
file_base_name(user,   'rsa-sha2-256'       ) -> "id_rsa";
file_base_name(user,   'rsa-sha2-384'       ) -> "id_rsa";
file_base_name(user,   'rsa-sha2-512'       ) -> "id_rsa";
file_base_name(user,   'ssh-dss'            ) -> "id_dsa";
file_base_name(user,   'ssh-ed25519'        ) -> "id_ed25519";
file_base_name(user,   'ssh-ed448'          ) -> "id_ed448";
file_base_name(user,   'ssh-rsa'            ) -> "id_rsa";
file_base_name(system, 'ecdsa-sha2-nistp256') -> "ssh_host_ecdsa_key";
file_base_name(system, 'ecdsa-sha2-nistp384') -> "ssh_host_ecdsa_key";
file_base_name(system, 'ecdsa-sha2-nistp521') -> "ssh_host_ecdsa_key";
file_base_name(system, 'rsa-sha2-256'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'rsa-sha2-384'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'rsa-sha2-512'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'ssh-dss'            ) -> "ssh_host_dsa_key";
file_base_name(system, 'ssh-ed25519'        ) -> "ssh_host_ed25519_key";
file_base_name(system, 'ssh-ed448'          ) -> "ssh_host_ed448_key";
file_base_name(system, 'ssh-rsa'            ) -> "ssh_host_rsa_key";
file_base_name(system, _                    ) -> "ssh_host_key".


identity_pass_phrase('ssh-dss'            ) -> dsa_pass_phrase;
identity_pass_phrase('ssh-rsa'            ) -> rsa_pass_phrase;
identity_pass_phrase('rsa-sha2-256'       ) -> rsa_pass_phrase;
identity_pass_phrase('rsa-sha2-384'       ) -> rsa_pass_phrase;
identity_pass_phrase('rsa-sha2-512'       ) -> rsa_pass_phrase;
identity_pass_phrase('ecdsa-sha2-nistp256') -> ecdsa_pass_phrase;
identity_pass_phrase('ecdsa-sha2-nistp384') -> ecdsa_pass_phrase;
identity_pass_phrase('ecdsa-sha2-nistp521') -> ecdsa_pass_phrase;
%% Not yet implemented: identity_pass_phrase('ssh-ed25519'   ) -> ed25519_pass_phrase;
%% Not yet implemented: identity_pass_phrase('ssh-ed448'     ) -> ed448_pass_phrase;
identity_pass_phrase(_) -> undefined.


%%%----------------------------------------------------------------
file_name(Type, Name, Opts) ->
    filename:join(ssh_dir(Type, Opts), Name).


%%%--------------------------------
ssh_dir({remoteuser, User}, Opts) ->
    %% server use this to find individual keys for an individual
    %% user when user tries to login with publickey
    case proplists:get_value(user_dir_fun, Opts) of
	undefined ->
            %% Try the local user instead
            ssh_dir(user, Opts);
	FUN ->
	    FUN(User)
    end;

ssh_dir(user, Opts) ->
    %% client use this to find client ssh keys
    case proplists:get_value(user_dir, Opts, false) of
	false -> default_user_dir();
	D -> D
    end;

ssh_dir(system, Opts) ->
    %% server use this to find server host keys
    proplists:get_value(system_dir, Opts, "/etc/ssh").

%%%--------------------------------
default_user_dir() ->
    try
	default_user_dir(os:getenv("HOME"))
    catch
	_:_ ->
	    default_user_dir(init:get_argument(home))
    end.

default_user_dir({ok,[[Home|_]]}) ->
    default_user_dir(Home);
default_user_dir(Home) when is_list(Home) ->
    UserDir = filename:join(Home, ".ssh"),
    ok = filelib:ensure_dir(filename:join(UserDir, "dummy")),
    UserDir.

%%%################################################################
get_key_part([<<"---- BEGIN SSH2 PUBLIC KEY ----">> | Lines0]) ->
    %% RFC 4716 format
    {KeyValues,Lines} = get_hdr_lines(Lines0, []),
    ExpectedEndLine = <<"---- END SSH2 PUBLIC KEY ----">>,
    {Key,RestLines} = get_body(Lines,ExpectedEndLine),
    {rfc4716, Key, KeyValues, RestLines};

get_key_part([<<"-----BEGIN ", Rest/binary>> | Lines0]) ->
    %% PEM format
    ExpectedEndLine = <<"-----END ",Rest/binary>>,
    [MiddlePart, <<>>] = binary:split(Rest,  <<" KEY-----">>),
    {KeyValues,Lines} = get_hdr_lines(Lines0, []),
    {Key,RestLines} = get_body(Lines,ExpectedEndLine),
    {asn1_type(MiddlePart), Key, KeyValues, RestLines}.


get_hdr_lines(Lines, Acc) ->
    Line1 = hd(Lines),
    case binary:split(Line1, <<":">>) of
        [Line1] ->
            {lists:reverse(Acc), Lines};
        [Key,Value] ->
            get_hdr_lines(tl(Lines), [{trim(Key),trim(Value)}|Acc])
    end.


get_body(Lines, ExpectedEndLine) ->
    {KeyPart, [ExpectedEndLine|RestLines]} = 
        lists:splitwith(fun(L) -> L=/=ExpectedEndLine end, Lines),
    {base64:mime_decode(iolist_to_binary(KeyPart)), RestLines}.

trim(<<" ",B/binary>>) -> trim(B);
trim(B) -> B.

asn1_type(<<"RSA PRIVATE">>) -> 'RSAPrivateKey';
asn1_type(<<"RSA PUBLIC">>) -> 'RSAPublicKey';
asn1_type(<<"DSA PRIVATE">>) -> 'DSAPrivateKey';
asn1_type(<<"EC PRIVATE">>) -> 'ECPrivateKey';
asn1_type(<<"OPENSSH PRIVATE">>) -> 'openssh-key-v1';
asn1_type(<<"PRIVATE">>) -> 'PrivateKeyInfo';
asn1_type(_) -> undefined.

%%%================================================================
%%% From https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.key
%%%

-define(NON_CRYPT_BLOCKSIZE, 8).

openssh_key_v1_decode(<<"openssh-key-v1",0,
                        ?DEC_BIN(CipherName, _L1),
                        ?DEC_BIN(KdfName, _L2),
                        ?DEC_BIN(KdfOptions, _L3),
                        ?UINT32(N),      % number of keys
                        Rest/binary
                      >>, Pwd) ->
    openssh_key_v1_decode(Rest, N, Pwd, CipherName, KdfName, KdfOptions, N, []).


openssh_key_v1_decode(<<?DEC_BIN(BinKey,_L1), Rest/binary>>, I,
                      Pwd, CipherName, KdfName, KdfOptions, N, PubKeyAcc) when I>0 ->
    PublicKey = ssh_message:ssh2_pubkey_decode(BinKey),
    openssh_key_v1_decode(Rest, I-1, Pwd, CipherName, KdfName, KdfOptions, N, [PublicKey|PubKeyAcc]);

openssh_key_v1_decode(<<?DEC_BIN(Encrypted,_L)>>,
                      0, Pwd, CipherName, KdfName, KdfOptions, N, PubKeyAccRev) ->
    PubKeys = lists:reverse(PubKeyAccRev),
    try
        Plain = decrypt_openssh_key_v1(Encrypted, KdfName, KdfOptions, CipherName, Pwd),
        openssh_key_v1_decode_priv_keys(Plain, N, N, [], [])
    of
        {PrivKeys, _Comments} ->
            lists:zip(PubKeys, PrivKeys)
            %% lists:zip3(PubKeys, PrivKeys,_ Comments))
    catch
        error:{decryption, DecryptError} ->
            error({decryption, DecryptError})
    end.


openssh_key_v1_decode_priv_keys(Bin, I, N, KeyAcc, CmntAcc) when I>0 ->
    {PrivKey, <<?DEC_BIN(Comment,_Lc),Rest/binary>>} = ssh_message:ssh2_privkey_decode2(Bin),
    openssh_key_v1_decode_priv_keys(Rest, I-1, N, [PrivKey|KeyAcc], [Comment|CmntAcc]);
openssh_key_v1_decode_priv_keys(_Padding, 0, _N, PrivKeyAccRev, CommentAccRev) ->
    {lists:reverse(PrivKeyAccRev),
     lists:reverse(CommentAccRev)}.


decrypt_openssh_key_v1(Encrypted, <<"none">>, <<>>, _CipherName, _Pwd) ->
    check_valid_decryption(Encrypted, ?NON_CRYPT_BLOCKSIZE);
decrypt_openssh_key_v1(Encrypted, <<>>, <<>>, _CipherName, _Pwd) ->
    check_valid_decryption(Encrypted, ?NON_CRYPT_BLOCKSIZE);
decrypt_openssh_key_v1(_Encrypted, <<"bcrypt">>, <<?DEC_BIN(_Salt,_L),?UINT32(_Rounds)>>, _CipherName, _Pwd) ->
    error({decryption, {not_supported,bcrypt}});
decrypt_openssh_key_v1(_Encrypted, KdfName, _KdfOpts, _CipherName, _Pwd) ->
    error({decryption, {not_supported,KdfName}}).


check_valid_decryption(<<?UINT32(Checkint1),?UINT32(Checkint2),Plain/binary>>, BlockSize) when Checkint2==Checkint1 ->
    case check_padding(Plain, BlockSize) of
        true ->
            Plain;
        false ->
            error({decryption,bad_padding})
    end;
check_valid_decryption(_, _) ->
    error({decryption,bad_result}).


check_padding(Bin, BlockSize) ->
    N = binary:last(Bin),
    if
        N < BlockSize ->
            %% Check that Bin is <<...,1,2,...,N>>
            Padding = binary:part(Bin, {byte_size(Bin),-N}),
            ExpectedPadding = list_to_binary(lists:seq(1,N)), % <<1,2,...,N>>
            Padding == ExpectedPadding;
        true ->
            true
    end.

%%%----------------------------------------------------------------
%% KeyPairs :: [ {Pub,Priv,Comment} ]
openssh_key_v1_encode(KeyPairs) ->
    CipherName = <<"none">>,
    BlockSize = ?NON_CRYPT_BLOCKSIZE, % Cipher dependent
    KdfName = <<"none">>,
    KdfOptions = <<>>,
    NumKeys = length(KeyPairs),
    CheckInt = crypto:strong_rand_bytes(4),
    UnEncrypted0 = <<CheckInt/binary,
                     CheckInt/binary,
                     (openssh_key_v1_encode_priv_keys_cmnts(KeyPairs))/binary>>,
    UnEncrypted = <<UnEncrypted0/binary,
                    (pad(byte_size(UnEncrypted0), BlockSize))/binary>>,
    Encrypted = encrypt_openssh_key_v1(UnEncrypted,  KdfName, KdfOptions, CipherName, ignore),
    <<"openssh-key-v1",0,
      ?STRING(CipherName),
      ?STRING(KdfName),
      ?STRING(KdfOptions),
      ?UINT32(NumKeys),
      (openssh_key_v1_encode_pub_keys(KeyPairs))/binary,
      ?STRING(Encrypted)>>.

%%%----
openssh_key_v1_encode_pub_keys(KeyPairs) ->
    openssh_key_v1_encode_pub_keys(KeyPairs, []).

openssh_key_v1_encode_pub_keys([{Priv = #'ECPrivateKey'{}, _Cmnt} | Ks], Acc) ->
    Pub = extract_public_key(Priv),
    Bk = ssh_message:ssh2_pubkey_encode(Pub),
    openssh_key_v1_encode_pub_keys(Ks, [<<?STRING(Bk)>>|Acc]);
openssh_key_v1_encode_pub_keys([{K,_,_C}|Ks], Acc) ->
    Bk = ssh_message:ssh2_pubkey_encode(K),
    openssh_key_v1_encode_pub_keys(Ks, [<<?STRING(Bk)>>|Acc]);
openssh_key_v1_encode_pub_keys([], Acc) ->
    list_to_binary(lists:reverse(Acc)).


%%%----
openssh_key_v1_encode_priv_keys_cmnts(KeyPairs) ->
    openssh_key_v1_encode_priv_keys_cmnts(KeyPairs, []).

openssh_key_v1_encode_priv_keys_cmnts([{K = #'ECPrivateKey'{}, C} | Ks], Acc) ->
    Bk = ssh_message:ssh2_privkey_encode(K),
    openssh_key_v1_encode_priv_keys_cmnts(Ks, [<<Bk/binary,?STRING(C)>>|Acc]);
openssh_key_v1_encode_priv_keys_cmnts([{_,K,C}|Ks], Acc) ->
    Bk = ssh_message:ssh2_privkey_encode(K),
    openssh_key_v1_encode_priv_keys_cmnts(Ks, [<<Bk/binary,?STRING(C)>>|Acc]);
openssh_key_v1_encode_priv_keys_cmnts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

encrypt_openssh_key_v1(UnEncrypted, <<"none">>, <<>>, _CipherName, _Pwd) ->
    UnEncrypted;
encrypt_openssh_key_v1(_UnEncrypted,  KdfName, _KdfOptions, _CipherName, _Pwd) ->
    error({decryption, {not_supported,KdfName}}).

pad(N, BlockSize) when N>BlockSize -> pad(N rem BlockSize, BlockSize);
pad(N, BlockSize) -> list_to_binary(lists:seq(1,BlockSize-N)).

%%%================================================================
%%%
split_in_nonempty_lines(Bin) ->
    skip_blank_lines_and_comments( split_in_lines(Bin) ).

split_in_lines(Bin) ->
    binary:split(Bin, [<<"\n">>,<<"\r\n">>], [global,trim_all]).

skip_blank_lines_and_comments(Lines) ->
    lists:filter(fun(<<"#",_/binary>>) ->
                         %% skip comments
                         false;
                    (L) ->
                         %% skip blank lines
                         re:run(L, "^(\t|\s)+$") == nomatch
                 end, Lines).
