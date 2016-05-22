%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
-module(otp_5872).
-export([?MODULE/0,chunk_request_body/3]).

?MODULE() ->
    ok.

chunk_request_body(Body,_ChunkSize,Acc) when Body == <<>>; Body == [] ->
    LastChunk = "0\r\n",
    lists:reverse(["\r\n",LastChunk|Acc]);
chunk_request_body(Body,ChunkSize,Acc) when binary(Body), size(Body) >= ChunkSize ->
    <<ChunkBody:ChunkSize/binary,
      Rest/binary>> = Body,
    Chunk = [ibrowse_lib:dec2hex(4,ChunkSize),"\r\n",ChunkBody,"\r\n"],
    chunk_request_body(Rest,ChunkSize,[Chunk|Acc]);
chunk_request_body(Body,_ChunkSize,Acc) when binary(Body) ->
    BodySize = size(Body),
    Chunk = [ibrowse_lib:dec2hex(4,BodySize),"\r\n",Body,"\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n",LastChunk,Chunk|Acc]);
chunk_request_body(Body,ChunkSize,Acc) when list(Body), length(Body) >= ChunkSize ->
    {ChunkBody,Rest} = ?MODULE:split_list_at(Body,ChunkSize),
    Chunk = [ibrowse_lib:dec2hex(4,ChunkSize),"\r\n",ChunkBody,"\r\n"],
    chunk_request_body(Rest,ChunkSize,[Chunk|Acc]);
chunk_request_body(Body,_ChunkSize,Acc) when list(Body) ->
    BodySize = length(Body),
    Chunk = [ibrowse_lib:dec2hex(4,BodySize),"\r\n",Body,"\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n",LastChunk,Chunk|Acc]).
