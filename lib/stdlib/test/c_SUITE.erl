%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(c_SUITE).
-export([all/1]).
-export([c_1/1, c_2/1, c_3/1, c_4/1, memory/1]).

-include("test_server.hrl").

-import(c, [c/2]).

all(doc) -> ["Test cases for the 'c' module."];
all(suite) ->
    [c_1, c_2, c_3, c_4, memory].

%%% Write output to a directory other than current directory:

c_1(doc) ->
    ["Checks that c:c works also with option 'outdir' [ticket OTP-1209]."];
c_1(suite) ->
    [];
c_1(Config) when list(Config) ->
    ?line R = filename:join(?config(data_dir, Config), "m.erl"),
    ?line W = ?config(priv_dir, Config),
    ?line Result = c(R,[{outdir,W}]),
    ?line {ok, m} = Result.

c_2(doc) ->
    ["Checks that c:c works also with option 'outdir' [ticket OTP-1209]."];
c_2(suite) ->
    [];
c_2(Config) when list(Config) ->
    ?line R = filename:join(?config(data_dir, Config), "m"),
    ?line W = ?config(priv_dir, Config),
    ?line Result = c(R,[{outdir,W}]),
    ?line {ok, m} = Result.


%%% Put results in current directory (or rather, change current dir
%%% to the output dir):

c_3(doc) ->
    ["Checks that c:c works also with option 'outdir' (same as current"
     "directory). [ticket OTP-1209]."];
c_3(suite) ->
    [];
c_3(Config) when list(Config) ->
    ?line R = filename:join(?config(data_dir, Config), "m.erl"),
    ?line W = ?config(priv_dir, Config),
    ?line file:set_cwd(W),
    ?line Result = c(R,[{outdir,W}]),
    ?line {ok, m} = Result.

c_4(doc) ->
    ["Checks that c:c works also with option 'outdir' (same as current"
     "directory). [ticket OTP-1209]."];
c_4(suite) ->
    [];
c_4(Config) when list(Config) ->
    ?line R = filename:join(?config(data_dir, Config), "m"),
    ?line W = ?config(priv_dir, Config),
    ?line file:set_cwd(W),
    ?line Result = c(R,[{outdir,W}]),
    ?line {ok, m} = Result.

memory(doc) ->
    ["Checks that c:memory/[0,1] returns consistent results."];
memory(suite) ->
    [];
memory(Config) when list(Config) ->
    try
	?line ML = c:memory(),
	?line T =  mget(total, ML),
	?line P =  mget(processes, ML),
	?line S =  mget(system, ML),
	?line A =  mget(atom, ML),
	?line AU = mget(atom_used, ML),
	?line B =  mget(binary, ML),
	?line C =  mget(code, ML),
	?line E =  mget(ets, ML),
	?line T = P + S,
	?line if S >= A + B + C + E -> ok end,
	?line if A >= AU -> ok end,
	?line ok
    catch
	error:notsup ->
	    ?line {skipped,
		   "erlang:memory/[0,1] and c:memory/[0,1] not supported"}
    end.

% Help function for c_SUITE:memory/1    
mget(K, L) ->
    ?line {value,{K,V}} = lists:keysearch(K, 1, L),
    ?line test_v(c:memory(K)), % Check that c:memory/1 also accept this
                               % argument and returns an integer (usally
                               % *not* the same as V).
    ?line test_v(V).

% Help function for c_SUITE:memory/1    
test_v(V) when integer(V) ->
    ?line V.
