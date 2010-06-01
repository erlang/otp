%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

%%% Description: Reading and writing of PEM type encoded files.
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
%%
%% The encoding is divided into lines separated by <NL>, and each line
%% is precisely 64 characters long (excluding the <NL> characters,
%% except the last line which 64 characters long or shorter. <NL> may
%% follow the last line.

-module(pubkey_pem).

-export([read_file/1, read_file/2, write_file/2, decode/2]).
-export([decode_key/2]).

-define(ENCODED_LINE_LENGTH, 64).

%%====================================================================
%% Internal application API
%%====================================================================
read_file(File) ->
    read_file(File, no_passwd).

read_file(File, Passwd) ->
    {ok, Bin} = file:read_file(File),
    decode(Bin, Passwd).

write_file(File, Ds) ->
    file:write_file(File, encode_file(Ds)).

decode_key({_Type, Bin, not_encrypted}, _) ->
    Bin;
decode_key({_Type, Bin, {Chipher,Salt}}, Password) ->
    decode_key(Bin, Password, Chipher, Salt).

decode(Bin, Passwd) ->
    decode_file(split_bin(Bin), Passwd).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

split_bin(Bin) ->
    split_bin(0, Bin).

split_bin(N, Bin) ->
    case Bin of
	<<Line:N/binary, "\r\n", Rest/binary>> ->
	    [Line | split_bin(0, Rest)];
	<<Line:N/binary, "\n", Rest/binary>> ->
	    [Line | split_bin(0, Rest)];
	<<Line:N/binary>> ->
	    [Line];
	_ ->
	    split_bin(N+1, Bin)
    end.

decode_file(Bin, Passwd) ->
    decode_file(Bin, [], [Passwd]).

decode_file([<<"-----BEGIN CERTIFICATE REQUEST-----", _/binary>>|Rest], Ens, Info) ->
    decode_file2(Rest, [], Ens, cert_req, Info);
decode_file([<<"-----BEGIN CERTIFICATE-----", _/binary>>|Rest], Ens, Info) ->
    decode_file2(Rest, [], Ens, cert, Info);
decode_file([<<"-----BEGIN RSA PRIVATE KEY-----", _/binary>>|Rest], Ens, Info) ->
    decode_file2(Rest, [], Ens, rsa_private_key, Info);
decode_file([<<"-----BEGIN DSA PRIVATE KEY-----", _/binary>>|Rest], Ens, Info) ->
    decode_file2(Rest, [], Ens, dsa_private_key, Info);
decode_file([<<"-----BEGIN DH PARAMETERS-----", _/binary>>|Rest], Ens, Info) ->
    decode_file2(Rest, [], Ens, dh_params, Info);
decode_file([_|Rest], Ens, Info) ->
    decode_file(Rest, Ens, Info);
decode_file([], Ens, _Info) ->
    {ok, lists:reverse(Ens)}.

decode_file2([<<"Proc-Type: 4,ENCRYPTED", _/binary>>| Rest0], RLs, Ens, Tag, Info0) ->
    [InfoLine|Rest] = Rest0,
    Info = dek_info(InfoLine, Info0),
    decode_file2(Rest, RLs, Ens, Tag, Info);
decode_file2([<<"-----END", _/binary>>| Rest], RLs, Ens, Tag, Info0) ->
    Cs = erlang:iolist_to_binary(lists:reverse(RLs)),
    Bin = base64:mime_decode(Cs),
    case Info0 of
	[Password, Cipher, SaltHex | Info1] ->
	    Salt = unhex(SaltHex),
	    Enc = {Cipher, Salt},
	    Decoded = decode_key(Bin, Password, Cipher, Salt),
	    decode_file(Rest, [{Tag, Decoded, Enc}| Ens], Info1);
	_ ->
	    decode_file(Rest, [{Tag, Bin, not_encrypted}| Ens], Info0)
    end;
decode_file2([L|Rest], RLs, Ens, Tag, Info0) ->
    decode_file2(Rest, [L|RLs], Ens, Tag, Info0);
decode_file2([], _, Ens, _, _) ->
    {ok, lists:reverse(Ens)}.

%% Support same as decode_file
encode_file(Ds) ->
    lists:map(
      fun({cert, Bin, not_encrypted}) -> 
	      %% PKIX (X.509)
	      ["-----BEGIN CERTIFICATE-----\n",
	       b64encode_and_split(Bin),
	       "-----END CERTIFICATE-----\n\n"];
	 ({cert_req, Bin, not_encrypted}) -> 
	      %% PKCS#10
	      ["-----BEGIN CERTIFICATE REQUEST-----\n",
	       b64encode_and_split(Bin),
	       "-----END CERTIFICATE REQUEST-----\n\n"];
	 ({rsa_private_key, Bin, not_encrypted}) -> 
	      %% PKCS#?
	      ["XXX Following key assumed not encrypted\n",
	       "-----BEGIN RSA PRIVATE KEY-----\n",
	       b64encode_and_split(Bin),
	       "-----END RSA PRIVATE KEY-----\n\n"];
	 ({dsa_private_key, Bin, not_encrypted}) -> 
	      %% PKCS#?
	      ["XXX Following key assumed not encrypted\n",
	       "-----BEGIN DSA PRIVATE KEY-----\n",
	       b64encode_and_split(Bin),
	       "-----END DSA PRIVATE KEY-----\n\n"]
      end, Ds).

dek_info(Line0, Info) ->
    Line = binary_to_list(Line0),
    [_, DekInfo0] = string:tokens(Line, ": "),
    DekInfo1 = string:tokens(DekInfo0, ",\n"), 
    Info ++ DekInfo1.

unhex(S) ->
    unhex(S, []).

unhex("", Acc) ->
    list_to_binary(lists:reverse(Acc));
unhex([D1, D2 | Rest], Acc) ->
    unhex(Rest, [erlang:list_to_integer([D1, D2], 16) | Acc]).

decode_key(Data, no_passwd, _Alg, _Salt) ->
    Data;
decode_key(Data, Password, "DES-CBC", Salt) ->
    Key = password_to_key(Password, Salt, 8),
    IV = Salt,
    crypto:des_cbc_decrypt(Key, IV, Data);
decode_key(Data,  Password, "DES-EDE3-CBC", Salt) ->
    Key = password_to_key(Password, Salt, 24),
    IV = Salt,
    <<Key1:8/binary, Key2:8/binary, Key3:8/binary>> = Key,
    crypto:des_ede3_cbc_decrypt(Key1, Key2, Key3, IV, Data).

password_to_key(Data, Salt, KeyLen) ->
    <<Key:KeyLen/binary, _/binary>> = 
	password_to_key(<<>>, Data, Salt, KeyLen, <<>>),
    Key.

password_to_key(_, _, _, Len, Acc) when Len =< 0 ->
    Acc;
password_to_key(Prev, Data, Salt, Len, Acc) ->
    M = crypto:md5([Prev, Data, Salt]),
    password_to_key(M, Data, Salt, Len - size(M), <<Acc/binary, M/binary>>).

b64encode_and_split(Bin) ->
    split_lines(base64:encode(Bin)).

split_lines(<<Text:?ENCODED_LINE_LENGTH/binary, Rest/binary>>) ->
    [Text, $\n | split_lines(Rest)];
split_lines(Bin) ->
    [Bin, $\n].

