%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
-module(bin_syntax_3).
-export([?MODULE/0,decode_integer/3]).

?MODULE() ->
    ok.

decode_integer(Len, <<B1:1,B2:7,Bs/binary>>, RemovedBytes) when B1 == 0 ->
     Bin = <<Skip:Len/unit:8, Buffer2/binary>> = <<B1:1,B2:7,Bs/binary>>,
     Size = size(Bin),
     <<Int:Size/unit:8>> = Bin,
     {Int,Buffer2,RemovedBytes};
decode_integer(Len,<<B1:1,B2:7,Bs/binary>>,RemovedBytes)  ->
      Bin = <<Skip:Len/unit:8,Buffer2/binary>> = <<B1:1,B2:7,Bs/binary>>,
      Size = size(Bin),
      <<N:Size/unit:8>> = <<B2,Bs/binary>>,
      Int = N - (1 bsl (8 * size(Bin) -1)),
     {Int,Buffer2,RemovedBytes}.
