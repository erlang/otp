%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(int_break_SUITE).

%% Test break points.

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 basic/1,cleanup/1]).

-export([auto_attach/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [basic, cleanup].

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


init_per_testcase(_Case, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Mod = ordsets1,
    {module,Mod} = int:i(filename:join(DataDir, Mod)),
    ok = io:format("Interpreted modules: ~p", [int:interpreted()]),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = io:format("Interpreted modules: ~p", [int:interpreted()]),
    ok.

%% Tests setting a few break points.
basic(Config) when list(Config) ->
    int:auto_attach([init], {?MODULE,auto_attach}),
    S1 = [] = ordsets1:new_set(),
    ok = i:ib(ordsets1, 86),
    S2 = [xxx] = ordsets1:add_element(xxx, S1),
    S3 = [xxx,y] = ordsets1:add_element(y, S2),
    ok = i:ib(ordsets1, union, 2),
    [xxx,y,z] = ordsets1:union(S3, [z]),
    All = [{{ordsets1,86}, _}, {{ordsets1,_},_}|_] = lists:sort(int:all_breaks()),
    [] = lists:sort(int:all_breaks(foobar)),
    All = lists:sort(int:all_breaks(ordsets1)),
    ok.

%% Make sure that the auto-attach flag is turned off.
cleanup(Config) when list(Config) ->
    int:auto_attach(false),
    ok.

auto_attach(Pid) ->
    {ok, Meta} = int:attached(Pid),
    io:format("Pid = ~p; Meta = ~p", [Pid,Meta]),
    link(Meta),
    attach_loop(Pid, Meta).

attach_loop(Pid, Meta) ->
    receive
	Msg ->
	    io:format("attached: ~p", [Msg]),
	    attach_cmd(Msg, Pid, Meta),
	    attach_loop(Pid, Meta)
    end.

attach_cmd({Meta,{break_at,ordsets1,36,2}}, _Pid, Meta) ->
    int:meta(Meta, continue);
attach_cmd({Meta,{break_at,ordsets1,87,_}}, _Pid, Meta) ->
    int:meta(Meta, continue);
attach_cmd({Meta,{break_at,ordsets1,89,_}}, _Pid, Meta) ->
    int:meta(Meta, continue);
attach_cmd({Meta,{break_at,ordsets1,Line,_}}, _Pid, Meta) when 107 =< Line, Line =< 115 ->
    int:meta(Meta, finish);
attach_cmd({Meta,{break_at,_Mod,_Line,_Other}}=Cmd, _Pid, Meta) ->
    io:format("attached: no action for ~p", [Cmd]);
attach_cmd(_, _Pid, _Meta) ->
    ok.
