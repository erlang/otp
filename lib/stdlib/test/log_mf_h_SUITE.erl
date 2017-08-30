%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(log_mf_h_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, test/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [test].

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



%%-----------------------------------------------------------------
%% This is actually very basic tests, maybe we could test some more
%% in the future...
%%-----------------------------------------------------------------

test(Config) when is_list(Config) ->
    {ok, Pid} = gen_event:start_link(),
    PrivDir = proplists:get_value(priv_dir, Config),
    Log1 = PrivDir ++ "/log1",
    ok = file:make_dir(Log1),
    Args1 = log_mf_h:init(Log1, 500, 3),
    gen_event:add_handler(Pid, log_mf_h, Args1),
    generate(Pid, 200),
    {ok, Files} = file:list_dir(Log1),
    true = lists:member("1", Files),
    true = lists:member("index", Files),
    false = lists:member("2", Files),
    generate(Pid, 2500),
    %% The documentation doesn't guarantee that syncing one request
    %% causes all previous ones to be finished too, but that seems to
    %% be the case. We need to be sure that the files exist when we
    %% look for them with 'list_dir'.
    gen_event:sync_notify(Pid, "end"),
    {ok, Files2} = file:list_dir(Log1),
    true = lists:member("1", Files2),
    true = lists:member("2", Files2),
    true = lists:member("3", Files2),
    false = lists:member("4", Files2),
    true = lists:member("index", Files2),
    {ok, #file_info{size=Size1,type=regular}} = file:read_file_info(Log1 ++ "/1"),
    if Size1 > 500 -> ct:fail({too_big, Size1});
       true -> ok end,
    {ok, #file_info{size=Size2,type=regular}} = file:read_file_info(Log1 ++ "/2"),
    if Size2 > 500 -> ct:fail({too_big, Size2});
       true -> ok end,
    {ok, #file_info{size=Size3,type=regular}} = file:read_file_info(Log1 ++ "/3"),
    if Size3 > 500 -> ct:fail({too_big, Size3});
       true -> ok end,
    gen_event:delete_handler(Pid, log_mf_h, []),
    {ok, Index} = read_index_file(Log1),
    gen_event:add_handler(Pid, log_mf_h, Args1),    
    X = if Index == 3 -> 1; true -> Index + 1 end,
    {ok, X} = read_index_file(Log1).


generate(Pid, Bytes) when Bytes > 32 ->
    gen_event:notify(Pid, make_list(32, [])),
    generate(Pid, Bytes - 32);
generate(_, _) -> ok.

make_list(0, Res) ->  Res;
make_list(N, Res) -> make_list(N-1, [67 | Res]).


read_index_file(Dir) ->
    case file:open(Dir ++ "/index", [read,raw]) of
	{ok, Fd} ->
	    case catch file:read(Fd, 1) of
		{ok, [Index]} -> {ok, Index};
		_ -> error
	    end;
	_ -> error
    end.
