%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

-module(ssl_pem).

%%% Purpose: Reading and writing of PEM type encoded files for SSL.

%% NB write_file/2 is only preliminary.

%% PEM encoded files have the following structure:
%%
%%	<text>
%%	-----BEGIN SOMETHING-----<CR><LF>
%%	<Base64 encoding line><CR><LF>
%%	<Base64 encoding line><CR><LF>
%%	...
%%	-----END SOMETHING-----<CR><LF>
%%	<text>
%%
%% A file can contain several BEGIN/END blocks. Text lines between
%% blocks are ignored.

-export([read_file/1, read_file/2, write_file/2]).

%% Read a PEM file and return each decoding as a binary. 

read_file(File) ->
    read_file(File, no_passwd).

read_file(File, Passwd) ->
    {ok, Fd} = file:open(File, [read]),
    Result = decode_file(Fd, Passwd),
    file:close(Fd),
    Result.

decode_file(Fd, Passwd) ->
    decode_file(Fd, [], [], notag, [Passwd]).

decode_file(Fd, _RLs, Ens, notag, Info) ->
    case io:get_line(Fd, "") of
	"-----BEGIN CERTIFICATE REQUEST-----" ++ _ ->
	    decode_file(Fd, [], Ens, cert_req, Info);
	"-----BEGIN CERTIFICATE-----" ++ _ ->
	    decode_file(Fd, [], Ens, cert, Info);
	"-----BEGIN RSA PRIVATE KEY-----" ++ _ ->
	    decode_file(Fd, [], Ens, rsa_private_key, Info);
	eof ->
	    {ok, lists:reverse(Ens)};
	_ ->
	    decode_file(Fd, [], Ens, notag, Info)
    end;
decode_file(Fd, RLs, Ens, Tag, Info0) ->
    case io:get_line(Fd, "") of
	"Proc-Type: 4,ENCRYPTED"++_ ->
	    Info = dek_info(Fd, Info0),
	    decode_file(Fd, RLs, Ens, Tag, Info);
	"-----END" ++ _ ->			% XXX sloppy
	    Cs = lists:flatten(lists:reverse(RLs)),
	    Bin = ssl_base64:join_decode(Cs),
	    case Info0 of
		[Password, Cipher, SaltHex | Info1] ->
		    Decoded = decode_key(Bin, Password, Cipher, unhex(SaltHex)),
		    decode_file(Fd, [], [{Tag, Decoded}| Ens], notag, Info1);
		_ ->
		    decode_file(Fd, [], [{Tag, Bin}| Ens], notag, Info0)
	    end;
	eof ->
	    {ok, lists:reverse(Ens)};
	L ->
	    decode_file(Fd, [L|RLs], Ens, Tag, Info0)
    end.

dek_info(Fd, Info) ->
    Line = io:get_line(Fd, ""),
    [_, DekInfo0] = string:tokens(Line, ": "),
    DekInfo1 = string:tokens(DekInfo0, ",\n"), 
    Info ++ DekInfo1.

unhex(S) ->
    unhex(S, []).

unhex("", Acc) ->
    lists:reverse(Acc);
unhex([D1, D2 | Rest], Acc) ->
    unhex(Rest, [erlang:list_to_integer([D1, D2], 16) | Acc]).

decode_key(Data, Password, "DES-CBC", Salt) ->
    Key = password_to_key(Password, Salt, 8),
    IV = Salt,
    crypto:des_cbc_decrypt(Key, IV, Data);
decode_key(Data,  Password, "DES-EDE3-CBC", Salt) ->
    Key = password_to_key(Password, Salt, 24),
    IV = Salt,
    <<Key1:8/binary, Key2:8/binary, Key3:8/binary>> = Key,
    crypto:des_ede3_cbc_decrypt(Key1, Key2, Key3, IV, Data).

write_file(File, Ds) ->
    file:write_file(File, encode_file(Ds)).

encode_file(Ds) ->
    [encode_file_1(D) || D <- Ds].

encode_file_1({cert, Bin}) -> 
    %% PKIX (X.509)
    ["-----BEGIN CERTIFICATE-----\n",
     ssl_base64:encode_split(Bin),
     "-----END CERTIFICATE-----\n\n"];
encode_file_1({cert_req, Bin}) -> 
    %% PKCS#10
    ["-----BEGIN CERTIFICATE REQUEST-----\n",
     ssl_base64:encode_split(Bin),
     "-----END CERTIFICATE REQUEST-----\n\n"];
encode_file_1({rsa_private_key, Bin}) -> 
    %% PKCS#?
    ["XXX Following key assumed not encrypted\n",
     "-----BEGIN RSA PRIVATE KEY-----\n",
     ssl_base64:encode_split(Bin),
     "-----END RSA PRIVATE KEY-----\n\n"].

password_to_key(Data, Salt, KeyLen) ->
    <<Key:KeyLen/binary, _/binary>> = 
	password_to_key(<<>>, Data, Salt, KeyLen, <<>>),
    Key.

password_to_key(_, _, _, Len, Acc) when Len =< 0 ->
    Acc;
password_to_key(Prev, Data, Salt, Len, Acc) ->
    M = crypto:md5([Prev, Data, Salt]),
    password_to_key(M, Data, Salt, Len - byte_size(M), <<Acc/binary, M/binary>>).
