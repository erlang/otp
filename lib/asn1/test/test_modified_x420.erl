%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(test_modified_x420).
-export([test/1]).

-include_lib("common_test/include/ct.hrl").

test(Config) ->
    DataDir = proplists:get_value(data_dir,Config),

    Der = read_pem(filename:join([DataDir,modified_x420,"p7_signed_data.pem"])),
    {ok,{_,_,SignedData}} = 'PKCS7':decode( 'ContentInfo', Der),
    {ok,_} = 'PKCS7':decode('SignedData', SignedData),
    ok.

read_pem(File) ->    
    {ok,Bin} = file:read_file(File),
    Der = base64:mime_decode(lists:flatten(extract_base64(Bin))),
    binary_to_list(Der).


extract_base64(Binary) ->
    extract_base64_lines(string:lexemes(binary_to_list(Binary), "\n")).

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
