%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
-module(test_modified_x420).

%-compile(export_all).
-export([compile/1, test_io/1]).

-include_lib("test_server/include/test_server.hrl").

compile(Config) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),

    ok = asn1ct:compile(filename:join([DataDir,modified_x420,"PKCS7"]),[der,{outdir,OutDir}]),
    ok = asn1ct:compile(filename:join([DataDir,modified_x420,"InformationFramework"]),[der,{outdir,OutDir}]),
    ok = asn1ct:compile(filename:join([DataDir,modified_x420,"AuthenticationFramework"]),[der,{outdir,OutDir}]).

test_io(Config) ->
    io:format("~p~n~n", [catch test(Config)]).

test(Config) ->
    ?line DataDir = ?config(data_dir,Config),
%    ?line OutDir = ?config(priv_dir,Config),

    ?line Der = read_pem(filename:join([DataDir,modified_x420,"p7_signed_data.pem"])),
    ?line {ok, {_,_,SignedData}} = 'PKCS7':decode('ContentInfo', Der),
    ?line {ok,_} = 'PKCS7':decode('SignedData', SignedData).

read_pem(File) ->    
    ?line {ok, Bin} = file:read_file(File),
    ?line ssl_base64:join_decode(lists:flatten(extract_base64(Bin))).



extract_base64(Binary) ->
    extract_base64_lines(string:tokens(binary_to_list(Binary), "\n")).

extract_base64_lines(["-----BEGIN"++_ | Lines]) ->
    take_base64_lines(Lines, _Acc = []);
extract_base64_lines([_|Lines]) ->
    extract_base64_lines(Lines);
extract_base64_lines([]) ->
    [].

take_base64_lines(["-----END"++_|_], Acc) ->
    lists:reverse(Acc);
take_base64_lines([L|Lines], Acc) ->
    take_base64_lines(Lines, [L|Acc]);
take_base64_lines([], Acc) ->
    lists:reverse(Acc).
