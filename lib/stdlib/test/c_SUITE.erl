%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(c_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([c_1/1, c_2/1, c_3/1, c_4/1, nc_1/1, nc_2/1, nc_3/1, nc_4/1,
	 c_default_outdir_1/1, c_default_outdir_2/1,
         nc_default_outdir_1/1, nc_default_outdir_2/1,
         ls/1, memory/1]).

-include_lib("common_test/include/ct.hrl").

-import(c, [c/2, nc/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [c_1, c_2, c_3, c_4, nc_1, nc_2, nc_3, nc_4,
     c_default_outdir_1, c_default_outdir_2,
     nc_default_outdir_1, nc_default_outdir_2,
     ls, memory].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%% Write output to a directory other than current directory:

%% OTP-1209: Check that c:c/2 works also with option 'outdir'.
c_1(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m.erl"),
    W = proplists:get_value(priv_dir, Config),
    Result = c(R,[{outdir,W}]),
    {ok, m} = Result.

%% OTP-1209: Check that c:c/2 works also with option 'outdir'.
c_2(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m"),
    W = proplists:get_value(priv_dir, Config),
    Result = c(R,[{outdir,W}]),
    {ok, m} = Result.


%%% Put results in current directory (or rather, change current dir
%%% to the output dir):

%% OTP-1209: Check that c:c/2 works also with option 'outdir'
%% (same as current directory).
c_3(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m.erl"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Result = c(R,[{outdir,W}]),
    {ok, m} = Result.

%% OTP-1209: Check that c:c/2 works also with option 'outdir'
%% (same as current directory).
c_4(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Result = c(R,[{outdir,W}]),
    {ok, m} = Result.

%%% Write output to a directory other than current directory:

%% Check that c:nc/2 works also with option 'outdir'.
nc_1(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m.erl"),
    W = proplists:get_value(priv_dir, Config),
    Result = nc(R,[{outdir,W}]),
    {ok, m} = Result.

%% Check that c:nc/2 works also with option 'outdir'.
nc_2(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m"),
    W = proplists:get_value(priv_dir, Config),
    Result = nc(R,[{outdir,W}]),
    {ok, m} = Result.


%%% Put results in current directory (or rather, change current dir
%%% to the output dir):

%% Check that c:nc/2 works also with option 'outdir'
%% (same as current directory).
nc_3(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m.erl"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Result = nc(R,[{outdir,W}]),
    {ok, m} = Result.

%% Check that c:nc/2 works also with option 'outdir'
%% (same as current directory).
nc_4(Config) when is_list(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Result = nc(R,[{outdir,W}]),
    {ok, m} = Result.

c_default_outdir_1(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m.erl"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Obj = "m" ++ code:objfile_extension(),
    _ = file:delete(Obj),
    false = filelib:is_file(Obj),
    Result = c:c(R),
    {ok, m} = Result,
    true = filelib:is_file(Obj).

c_default_outdir_2(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Obj = "m" ++ code:objfile_extension(),
    _ = file:delete(Obj),
    false = filelib:is_file(Obj),
    Result = c:c(R),
    {ok, m} = Result,
    true = filelib:is_file(Obj).

nc_default_outdir_1(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m.erl"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Obj = "m" ++ code:objfile_extension(),
    _ = file:delete(Obj),
    false = filelib:is_file(Obj),
    Result = c:nc(R),
    {ok, m} = Result,
    true = filelib:is_file(Obj).

nc_default_outdir_2(Config) ->
    R = filename:join(proplists:get_value(data_dir, Config), "m"),
    W = proplists:get_value(priv_dir, Config),
    file:set_cwd(W),
    Obj = "m" ++ code:objfile_extension(),
    _ = file:delete(Obj),
    false = filelib:is_file(Obj),
    Result = c:nc(R),
    {ok, m} = Result,
    true = filelib:is_file(Obj).

ls(Config) when is_list(Config) ->
    Directory = proplists:get_value(data_dir, Config),
    ok = c:ls(Directory),
    File = filename:join(Directory, "m.erl"),
    ok = c:ls(File),
    ok = c:ls("no_such_file").

%% Check that c:memory/[0,1] returns consistent results.
memory(Config) when is_list(Config) ->
    try
	ML = c:memory(),
	T =  mget(total, ML),
	P =  mget(processes, ML),
	S =  mget(system, ML),
	A =  mget(atom, ML),
	AU = mget(atom_used, ML),
	B =  mget(binary, ML),
	C =  mget(code, ML),
	E =  mget(ets, ML),
	T = P + S,
	if S >= A + B + C + E -> ok end,
	if A >= AU -> ok end,
	ok
    catch
	error:notsup ->
	    {skipped,
	     "erlang:memory/[0,1] and c:memory/[0,1] not supported"}
    end.

%% Help function for c_SUITE:memory/1
mget(K, L) ->
    {value,{K,V}} = lists:keysearch(K, 1, L),
    test_v(c:memory(K)), % Check that c:memory/1 also accept this
						% argument and returns an integer (usally
						% *not* the same as V).
    test_v(V).

%% Help function for c_SUITE:memory/1
test_v(V) when is_integer(V) ->
    V.
