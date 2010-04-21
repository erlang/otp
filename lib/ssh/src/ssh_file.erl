%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
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

%%% Description: SSH file handling

-module(ssh_file).

-include("ssh.hrl").
-include("PKCS-1.hrl").
-include("DSS.hrl").

-export([public_host_dsa_key/2,private_host_dsa_key/2,
	 public_host_rsa_key/2,private_host_rsa_key/2,
	 public_host_key/2,private_host_key/2,
	 lookup_host_key/3, add_host_key/3, % del_host_key/2,
	 lookup_user_key/3, ssh_dir/2, file_name/3]).

-export([private_identity_key/2]).
%% , public_identity_key/2,
%% 	 identity_keys/2]).

-export([encode_public_key/1, decode_public_key_v2/2]).

-import(lists, [reverse/1, append/1]).

-define(DBG_PATHS, true).

%% API
public_host_dsa_key(Type, Opts) ->
    File = file_name(Type, "ssh_host_dsa_key.pub", Opts),
    read_public_key_v2(File, "ssh-dss").

private_host_dsa_key(Type, Opts) ->
    File = file_name(Type, "ssh_host_dsa_key", Opts),
    read_private_key_v2(File, "ssh-dss").

public_host_rsa_key(Type, Opts) ->
    File = file_name(Type, "ssh_host_rsa_key.pub", Opts),
    read_public_key_v2(File, "ssh-rsa").

private_host_rsa_key(Type, Opts) ->
    File = file_name(Type, "ssh_host_rsa_key", Opts),
    read_private_key_v2(File, "ssh-rsa").

public_host_key(Type, Opts) ->
    File = file_name(Type, "ssh_host_key", Opts),
    case read_private_key_v1(File,public) of
	{error, enoent} ->	
	    read_public_key_v1(File++".pub");
	Result ->
	    Result
    end.
	    

private_host_key(Type, Opts) ->
    File = file_name(Type, "ssh_host_key", Opts),
    read_private_key_v1(File,private).



%% in: "host" out: "host,1.2.3.4.
add_ip(Host)                                                             ->
    case inet:getaddr(Host, inet) of
	{ok, Addr} ->
	    case ssh_connection:encode_ip(Addr) of
		false -> Host;
		IPString -> Host ++ "," ++ IPString
	    end;
	_ -> Host
    end.    

replace_localhost("localhost") ->
    {ok, Hostname} = inet:gethostname(),
    Hostname;
replace_localhost(Host) ->
    Host.

%% lookup_host_key
%% return {ok, Key(s)} or {error, not_found}
%%

lookup_host_key(Host, Alg, Opts) ->
    Host1 = replace_localhost(Host),
    do_lookup_host_key(Host1, Alg, Opts).
	    
do_lookup_host_key(Host, Alg, Opts) ->
    case file:open(file_name(user, "known_hosts", Opts), [read]) of
	{ok, Fd} ->
	    Res = lookup_host_key_fd(Fd, Host, Alg),
	    file:close(Fd),
	    Res;
	{error, enoent} -> {error, not_found};
	Error -> Error
    end.

add_host_key(Host, Key, Opts) ->
    Host1 = add_ip(replace_localhost(Host)),
    case file:open(file_name(user, "known_hosts", Opts),[write,append]) of
   	{ok, Fd} ->
   	    Res = add_key_fd(Fd, Host1, Key),
   	    file:close(Fd),
   	    Res;
   	Error ->
   	    Error
    end.

%% del_host_key(Host, Opts) ->
%%     Host1 = replace_localhost(Host),
%%     case file:open(file_name(user, "known_hosts", Opts),[write,read]) of
%% 	{ok, Fd} ->
%% 	    Res = del_key_fd(Fd, Host1),
%% 	    file:close(Fd),
%% 	    Res;
%% 	Error ->
%% 	    Error
%%     end.

identity_key_filename("ssh-dss") -> "id_dsa";
identity_key_filename("ssh-rsa") -> "id_rsa".

private_identity_key(Alg, Opts) ->
    Path = file_name(user, identity_key_filename(Alg), Opts),
    read_private_key_v2(Path, Alg).

read_public_key_v2(File, Type) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    List = binary_to_list(Bin),
	    case lists:prefix(Type, List) of
		true ->
		    List1 = lists:nthtail(length(Type), List),
		    K_S = ssh_bits:b64_decode(List1),
		    decode_public_key_v2(K_S, Type);
		false ->
		    {error, bad_format}
	    end;
	Error ->
	    Error
    end.

decode_public_key_v2(K_S, "ssh-rsa") ->
    case ssh_bits:decode(K_S,[string,mpint,mpint]) of
	["ssh-rsa", E, N] ->
	    {ok, #ssh_key { type = rsa,
			    public = {N,E},
			    comment=""}};
	_ ->
	    {error, bad_format}
    end;
decode_public_key_v2(K_S, "ssh-dss") ->
    case ssh_bits:decode(K_S,[string,mpint,mpint,mpint,mpint]) of
	["ssh-dss",P,Q,G,Y] ->
	    {ok,#ssh_key { type = dsa,
			   public = {P,Q,G,Y}
			  }};
	_A ->
	    {error, bad_format}
    end;
decode_public_key_v2(_, _) ->
    {error, bad_format}.
    

read_public_key_v1(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    List = binary_to_list(Bin),
	    case io_lib:fread("~d ~d ~d ~s", List) of
		{ok,[_Sz,E,N,Comment],_} ->
		    {ok,#ssh_key { type = rsa,
				   public ={N,E},
				   comment = Comment }};
		_Error ->
		    {error, bad_format}
	    end;
	Error ->
	    Error
    end.

%% pem_type("ssh-dss") -> "DSA";
%% pem_type("ssh-rsa") -> "RSA".

read_private_key_v2(File, Type) ->
     case catch (public_key:pem_to_der(File)) of
	 {ok, [{_, Bin, not_encrypted}]} ->
	     decode_private_key_v2(Bin, Type);
	 Error -> %% Note we do not handle password encrypted keys at the moment
	     {error, Error}
     end.
%%  case file:read_file(File) of
%% 	{ok,Bin} ->
%% 	    case read_pem(binary_to_list(Bin), pem_type(Type)) of
%% 		{ok,Bin1} ->
%% 		    decode_private_key_v2(Bin1, Type);
%% 		Error ->
%% 		    Error
%% 	    end;
%% 	Error ->
%% 	    Error
%%     end.

decode_private_key_v2(Private,"ssh-rsa") ->
    case 'PKCS-1':decode( 'RSAPrivateKey', Private) of
	{ok,RSA} -> %% FIXME Check for two-prime version
	    {ok, #ssh_key { type = rsa,
			    public = {RSA#'RSAPrivateKey'.modulus,
				      RSA#'RSAPrivateKey'.publicExponent},
			    private = {RSA#'RSAPrivateKey'.modulus,
				       RSA#'RSAPrivateKey'.privateExponent}
			    }};
	Error ->
	    Error
    end;
decode_private_key_v2(Private, "ssh-dss") ->
    case 'DSS':decode('DSAPrivateKey', Private) of
	{ok,DSA} -> %% FIXME Check for two-prime version
	    {ok, #ssh_key { type = dsa,
			    public = {DSA#'DSAPrivateKey'.p,
				      DSA#'DSAPrivateKey'.q,
				      DSA#'DSAPrivateKey'.g,
				      DSA#'DSAPrivateKey'.y},
			    private= {DSA#'DSAPrivateKey'.p,
				      DSA#'DSAPrivateKey'.q,
				      DSA#'DSAPrivateKey'.g,
				      DSA#'DSAPrivateKey'.x}
			   }};
	_ ->
	    {error,bad_format}
    end.

%% SSH1 private key format
%%  <<"SSH PRIVATE KEY FILE FORMATE 1.1\n" 0:8 
%%    CipherNum:8, Reserved:32,
%%    NSz/uint32, N/bignum, E/bignum, Comment/string,
%%
%% [ R0:8 R1:8 R0:8 R1:8, D/bignum, IQMP/bignum, Q/bignum, P/bignum, Pad(8)]>>
%%
%% where [ ] is encrypted using des3 (ssh1 version) and
%% a posssibly empty pass phrase using md5(passphase) as key
%% 

read_private_key_v1(File, Type) ->
    case file:read_file(File) of
	{ok,<<"SSH PRIVATE KEY FILE FORMAT 1.1\n",0,
	     CipherNum,_Resereved:32,Bin/binary>>} ->
	    decode_private_key_v1(Bin, CipherNum,Type);
	{ok,_} ->
	    {error, bad_format};
	Error ->
	    Error
    end.

decode_private_key_v1(Bin, CipherNum, Type) ->
    case ssh_bits:decode(Bin,0,[uint32, bignum, bignum, string]) of
	{Offset,[_NSz,N,E,Comment]} ->
	    if Type == public ->
		    {ok,#ssh_key { type=rsa,
				   public={N,E},
				   comment=Comment}};
	       Type == private ->
		    <<_:Offset/binary, Encrypted/binary>> = Bin,
		    case ssh_bits:decode(decrypt1(Encrypted, CipherNum),0,
					 [uint32, bignum, bignum, 
					  bignum, bignum,{pad,8}]) of
			{_,[_,D,IQMP,Q,P]} ->
			    {ok,#ssh_key { type=rsa,
					   public={N,E},
					   private={D,IQMP,Q,P},
					   comment=Comment}};
			_ ->
			    {error,bad_format}
		    end
	    end;
	_ ->
	    {error,bad_format}
    end.


decrypt1(Bin, CipherNum) ->
    decrypt1(Bin, CipherNum,"").

decrypt1(Bin, CipherNum, Phrase) ->
    if CipherNum == ?SSH_CIPHER_NONE; Phrase == "" ->
	    Bin;
       CipherNum == ?SSH_CIPHER_3DES ->
	    <<K1:8/binary, K2:8/binary>> = erlang:md5(Phrase),
	    K3 = K1,
	    IV = <<0,0,0,0,0,0,0,0>>,
	    Bin1 = crypto:des_cbc_decrypt(K3,IV,Bin),
	    Bin2 = crypto:des_cbc_encrypt(K2,IV,Bin1),
	    crypto:des_cbc_decrypt(K1,IV,Bin2)
    end.

%% encrypt1(Bin, CipherNum) ->
%%     encrypt1(Bin, CipherNum,"").

%% encrypt1(Bin, CipherNum, Phrase) ->
%%     if CipherNum == ?SSH_CIPHER_NONE; Phrase == "" ->
%% 	    Bin;
%%        CipherNum == ?SSH_CIPHER_3DES ->
%% 	    <<K1:8/binary, K2:8/binary>> = erlang:md5(Phrase),
%% 	    K3 = K1,
%% 	    IV = <<0,0,0,0,0,0,0,0>>,
%% 	    Bin1 = crypto:des_cbc_encrypt(K1,IV,Bin),
%% 	    Bin2 = crypto:des_cbc_decrypt(K2,IV,Bin1),
%% 	    crypto:des_cbc_encrypt(K3,IV,Bin2)
%%     end.

lookup_host_key_fd(Fd, Host, Alg) ->
    case io:get_line(Fd, '') of
	eof ->
	    {error, not_found};
	Line ->
	    case string:tokens(Line, " ") of
		[HostList, Alg, KeyData] ->
%% 		    io:format(" ~p lookup_host_key_fd: HostList ~p Alg ~p KeyData ~p\n",
%%			      [Host, HostList, Alg, KeyData]),
		    case lists:member(Host, string:tokens(HostList, ",")) of
			true ->
			    decode_public_key_v2(ssh_bits:b64_decode(KeyData), Alg);
			false ->
			    lookup_host_key_fd(Fd, Host, Alg)
		    end;
		_ ->
		    lookup_host_key_fd(Fd, Host, Alg)
	    end
    end.



%% del_key_fd(Fd, Host) ->
%%     del_key_fd(Fd, Host, 0, 0).

%% del_key_fd(Fd, Host, ReadPos0, WritePos0) ->
%%     case io:get_line(Fd, '') of
%% 	eof ->
%% 	    if ReadPos0 == WritePos0 ->
%% 		    ok;
%% 	       true ->
%% 		    file:truncate(Fd)
%% 	    end;
%% 	Line ->
%% 	    {ok,ReadPos1} = file:position(Fd, cur),
%% 	    case string:tokens(Line, " ") of
%% 		[HostList, _Type, _KeyData] ->
%% 		    case lists:member(Host, string:tokens(HostList, ",")) of
%% 			true ->
%% 			    del_key_fd(Fd, Host, ReadPos1, WritePos0);
%% 			false ->
%% 			    if ReadPos0 == WritePos0 ->
%% 				    del_key_fd(Fd, Host, ReadPos1, ReadPos1);
%% 			       true ->
%% 				    file:position(Fd, WritePos0),
%% 				    file:write(Fd, Line),
%% 				    {ok,WritePos1} = file:position(Fd,cur),
%% 				    del_key_fd(Fd, Host, ReadPos1, WritePos1)
%% 			    end
%% 		    end;
%% 		_ ->
%% 		    if ReadPos0 == WritePos0 ->
%% 			    del_key_fd(Fd, Host, ReadPos1, ReadPos1);
%% 		       true ->
%% 			    file:position(Fd, WritePos0),
%% 			    file:write(Fd, Line),
%% 			    {ok,WritePos1} = file:position(Fd,cur),
%% 			    del_key_fd(Fd, Host, ReadPos1, WritePos1)
%% 		    end		    
%% 	    end
%%     end.


add_key_fd(Fd, Host, Key) ->
    case Key#ssh_key.type of
	rsa ->
	    {N,E} = Key#ssh_key.public,
	    DK = ssh_bits:b64_encode(
		   ssh_bits:encode(["ssh-rsa",E,N],
				   [string,mpint,mpint])),
	    file:write(Fd, [Host, " ssh-rsa ", DK, "\n"]);
	dsa ->
	    {P,Q,G,Y} = Key#ssh_key.public,
	    DK = ssh_bits:b64_encode(
		   ssh_bits:encode(["ssh-dss",P,Q,G,Y],
				   [string,mpint,mpint,mpint,mpint])),
	    file:write(Fd, [Host, " ssh-dss ", DK, "\n"])
    end.


%% read_pem(Cs, Type) ->
%%     case read_line(Cs) of
%% 	{"-----BEGIN "++Rest,Cs1} ->
%% 	    case string:tokens(Rest, " ") of
%% 		[Type, "PRIVATE", "KEY-----"] ->
%% 		    read_pem64(Cs1, [], Type);
%% 		_ ->
%% 		    {error, bad_format}
%% 	    end;
%% 	{"",Cs1} when Cs1 =/= "" ->
%% 	    read_pem(Cs1,Type);
%% 	{_,""} ->
%% 	    {error, bad_format}
%%     end.

%% read_pem64(Cs, Acc, Type) ->
%%     case read_line(Cs) of
%% 	{"-----END "++Rest,_Cs1} ->
%% 	    case string:tokens(Rest, " ") of
%% 		[Type, "PRIVATE", "KEY-----"] ->
%% 		    {ok,ssh_bits:b64_decode(append(reverse(Acc)))};
%% 		Toks ->
%% 		    error_logger:format("ssh: TOKENS=~p\n", [Toks]),
%% 		    {error, bad_format}
%% 	    end;
%% 	{B64, Cs1} when Cs1 =/= "" ->
%% 	    read_pem64(Cs1, [B64|Acc], Type);
%% 	_What ->
%% 	    {error, bad_format}
%%     end.


%% read_line(Cs) -> read_line(Cs,[]).
%% read_line([$\r,$\n|T], Acc) -> {reverse(Acc), T};
%% read_line([$\n|T], Acc) -> {reverse(Acc), T};
%% read_line([C|T], Acc) -> read_line(T,[C|Acc]);
%% read_line([], Acc) -> {reverse(Acc),[]}.

lookup_user_key(User, Alg, Opts) ->
    SshDir = ssh_dir({remoteuser,User}, Opts),
    case lookup_user_key_f(User, SshDir, Alg, "authorized_keys", Opts) of
	{ok, Key} ->
	    {ok, Key};
	_ ->
	    lookup_user_key_f(User, SshDir, Alg,  "authorized_keys2", Opts)
    end.

lookup_user_key_f(_User, [], _Alg, _F, _Opts) ->
    {error, nouserdir};
lookup_user_key_f(_User, nouserdir, _Alg, _F, _Opts) ->
    {error, nouserdir};
lookup_user_key_f(_User, Dir, Alg, F, _Opts) ->
    FileName = filename:join(Dir, F),
    case file:open(FileName, [read]) of
	{ok, Fd} ->
	    Res = lookup_user_key_fd(Fd, Alg),
	    file:close(Fd),
	    Res;
	{error, Reason} ->
	    {error, {{openerr, Reason}, {file, FileName}}}
    end.

lookup_user_key_fd(Fd, Alg) ->
    case io:get_line(Fd, '') of
	eof ->
	    {error, not_found};
	Line ->
	    case string:tokens(Line, " ") of
		[Alg, KeyData, _] ->
		    %% 		    io:format("lookup_user_key_fd: HostList ~p Alg ~p KeyData ~p\n",
		    %% 			      [HostList, Alg, KeyData]),
		    decode_public_key_v2(ssh_bits:b64_decode(KeyData), Alg);
		_Other ->
		    %%?dbg(false, "key_fd Other: ~w ~w\n", [Alg, _Other]),
		    lookup_user_key_fd(Fd, Alg)
	    end
    end.


encode_public_key(#ssh_key{type = rsa, public = {N, E}}) ->
    ssh_bits:encode(["ssh-rsa",E,N],
		    [string,mpint,mpint]);
encode_public_key(#ssh_key{type = dsa, public = {P,Q,G,Y}}) ->
    ssh_bits:encode(["ssh-dss",P,Q,G,Y],
		    [string,mpint,mpint,mpint,mpint]).

%%
%% Utils
%%

%% server use this to find individual keys for
%% an individual user when user tries to login
%% with publickey
ssh_dir({remoteuser, User}, Opts) ->
    case proplists:get_value(user_dir_fun, Opts) of
	undefined ->
	    case proplists:get_value(user_dir, Opts) of
		undefined ->
		    default_user_dir();
		Dir ->
		    Dir
	    end;
	FUN ->
	    FUN(User)
    end;

%% client use this to find client ssh keys
ssh_dir(user, Opts) ->
    case proplists:get_value(user_dir, Opts, false) of
	false -> default_user_dir();
	D -> D
    end;

%% server use this to find server host keys
ssh_dir(system, Opts) ->
    proplists:get_value(system_dir, Opts, "/etc/ssh").

file_name(Type, Name, Opts) ->
    FN = filename:join(ssh_dir(Type, Opts), Name),
    %%?dbg(?DBG_PATHS, "file_name: ~p\n", [FN]),
    FN.

default_user_dir()->
    {ok,[[Home|_]]} = init:get_argument(home),
    filename:join(Home, ".ssh").
