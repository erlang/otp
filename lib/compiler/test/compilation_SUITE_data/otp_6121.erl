%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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
