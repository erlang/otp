%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
-module(binary).
%%
%% The following functions implemented as BIF's
%%  binary:compile_pattern/1
%%  binary:find/3
%% XXX:PaN more to come...

-export([first/1,first/2,last/1,last/2,nth/2,extract/3]).

first(<<F:1/binary,_/binary>>) ->
    F;
first(_) ->
    erlang:error(badarg).
first(N,Bin) when is_integer(N), N >= 0 ->
    case Bin of
	<<F:N/binary,_/binary>> ->
	    F;
	_ ->
	    erlang:error(badarg)
    end;
first(_,_) ->
    erlang:error(badarg).
last(<<>>) ->
    erlang:error(badarg);
last(Bin) when is_binary(Bin) ->
    Sz = byte_size(Bin) - 1,
    <<_:Sz/binary,L:1/binary>> = Bin,
    L;
last(_) ->
    erlang:error(badarg).
last(N,Bin) when is_integer(N), N >= 0, is_binary(Bin) ->
    Sz = byte_size(Bin) - N,
    case Bin of
	<<_:Sz/binary,L:N/binary>> ->
	    L;
	_ ->
	    erlang:error(badarg)
    end;
last(_,_) ->
    erlang:error(badarg).

nth(N, Bin) when is_integer(N), N > 0 ->
    M = N - 1,
    case Bin of
	<<_:M/binary,V:1/binary,_/binary>> ->
	    V;
	_ ->
	    erlang:error(badarg)
    end;
nth(_,_) ->
    erlang:error(badarg).

extract(N,Size,Bin) when is_integer(N), is_integer(Size), is_binary(Bin) ->
    M = N - 1,
    case Bin of
	<<_:M/binary,V:Size/binary,_/binary>> ->
	    V;
	_ ->
	    erlang:error(badarg)
    end;
extract(_,_,_) ->
    erlang:error(badarg).
