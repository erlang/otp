%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
-export([test/1]).

-include_lib("test_server/include/test_server.hrl").

test(Config) ->
    DataDir = ?config(data_dir,Config),

    Der = read_pem(filename:join([DataDir,modified_x420,"p7_signed_data.pem"])),
    {ok,{_,_,SignedData}} = 'PKCS7':decode( 'ContentInfo', Der),
    {ok,_} = 'PKCS7':decode('SignedData', SignedData),
    ok.

read_pem(File) ->    
    {ok,Bin} = file:read_file(File),
    Der = base64:mime_decode(lists:flatten(extract_base64(Bin))),
    binary_to_list(Der).


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
