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
-module(otp_6121).
-export([?MODULE/0]).

?MODULE() ->
    42 = digit_map_timer(<<1>>, 42),
    test(),
    Beam = code:which(?MODULE),
    Sz = filelib:file_size(Beam),
    io:format("Size of Beam file: ~p\n", [Sz]),
    if 
	100 < Sz, Sz < 100000 ->
	    ok
    end.

test() ->
    %% Make sure that the compiler does not make an unreasonable
    %% expansion when trying to optimize the following expressions.
    <<0:(8*128*1024)>> = id(<<0:(8*128*1024)>>),
    <<100:(8*128*1024)>> = id(<<100:(8*128*1024)>>),
    <<1009797879398749873879789879388:(8*128*1024)>> = 
 	id(<<1009797879398749873879789879388:(8*128*1024)>>),
    <<7:(8*128*1024)/little>> = id(<<7:(8*128*1024)/little>>),
    ok.

id(I) -> I.

digit_map_timer(<<_:8>>, Int) when is_integer(Int) andalso Int >= 0 ->
    Int;
digit_map_timer(_, _) ->
    error.
