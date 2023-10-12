%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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
         basic/1,delete_breakpoints/1]).

-export([auto_attach/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [basic,delete_breakpoints].

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
    ok = int:auto_attach(false),
    ok = int:no_break(),
    ok = int:clear(),
    ok.

%% Test auto_attach functions and setting breakpoints.
basic(_Config) ->
    int:auto_attach([init], {?MODULE,auto_attach}),
    S1 = [] = ordsets1:new_set(),
    ok = i:ib(ordsets1, 86),                    %Set one breakpoint.
    S2 = [xxx] = ordsets1:add_element(xxx, S1),
    S3 = [xxx,y] = ordsets1:add_element(y, S2),
    ok = i:ib(ordsets1, union, 2),              %Set five breakpoints.
    [xxx,y,z] = ordsets1:union(S3, [z]),
    All = lists:sort(int:all_breaks()),
    [{{ordsets1,86}, _}, {{ordsets1,_},_}|_] = All,
    6 = length(All),
    [] = lists:sort(int:all_breaks(foobar)),
    All = lists:sort(int:all_breaks(ordsets1)),
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
attach_cmd({Meta,{break_at,ordsets1,Line,_}}, _Pid, Meta) when 107 =< Line, Line =< 115 ->
    int:meta(Meta, finish);
attach_cmd({Meta,{break_at,_Mod,_Line,_Other}}=Cmd, _Pid, Meta) ->
    io:format("attached: no action for ~p", [Cmd]);
attach_cmd(_, _Pid, _Meta) ->
    ok.

%% Test that deleting breakpoints works.
delete_breakpoints(_Config) ->
    Mod = ordsets1,

    %% Testing deleting all breakpoints in all modules.
    ok = i:ib(Mod, intersection, 2),            %Set five breakpoints.
    5 = num_breaks(),
    ok = int:no_break(),
    0 = num_breaks(),
    [b] = Mod:intersection([a,b,c], [b,d,e]),

    %% Set 10 breakpoints.
    ok = i:ib(Mod, 89),                         %One breakpoint.
    1 = num_breaks(),
    ok = i:ib(Mod, union, 2),                   %Five breakpoints.
    6 = num_breaks(),
    ok = i:ib(Mod, del_element, 2),             %Four breakpoints.
    10 = num_breaks(),

    %% Make sure that all breakpoints remain when deleting breakpoints
    %% for another (non-existing) module.
    ok = int:no_break(foobar),
    [] = int:all_breaks(foobar),
    10 = num_breaks(),

    %% Delete the breakpoint in ordsets1:add_element/2, line 89,
    %% and testing calling it. If the breakpoint has not been removed
    %% the call will hang and the test case will fail with a timetrap
    %% timeout.
    ok = int:delete_break(Mod, 89),
    9 = num_breaks(),
    [x] = Mod:add_element(x, []),

    %% Delete all breakpoints in ordsets1:del_element/2.
    ok = int:del_break_in(Mod, del_element, 2),
    5 = num_breaks(),
    [a,b,d,e] = Mod:del_element(c, [a,b,c,d,e]),

    %% GH-7336: Deleting all breakpoints for a module didn't work.
    ok = int:no_break(Mod),
    [] = int:all_breaks(Mod),
    [] = int:all_breaks(),

    %% All breakpoints should now be removed.
    [x] = Mod:add_element(x, []),
    [a,b,d,e] = Mod:del_element(c, [a,b,c,d,e]),
    [a,b,c] = Mod:union([a,c], [a,b]),

    ok.

%% Return the number of breakpoints in the ordsets1 module.
num_breaks() ->
    %% Assumption: There are breakpoints only in the ordsets1 module.
    Breaks = lists:sort(int:all_breaks()),
    Breaks = lists:sort(int:all_breaks(ordsets1)),
    length(Breaks).
