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

%%
-module(mnesia_registry_test).
-author('hakan@erix.ericsson.se').
-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([good_dump/1, bad_dump/1, dump_registry/2, restore_registry/2]).

-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [good_dump, bad_dump].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
good_dump(doc) ->
    ["Dump a faked C-node registry"];
good_dump(suite) -> [];
good_dump(Config) when is_list(Config) ->
    [Node] = Nodes = ?acquire_nodes(1, Config),
    T1 = gordon,
    ?match(ok, mnesia_registry:create_table(T1)),
    One = {T1, 1, 0, integer, 0, 10},
    Two = {T1, "two", 3, integer, 0, 20},
    Three = {T1, 3, 0, string, 6, "thirty"},
    ?match(ok, mnesia:dirty_write(One)),
    ?match(ok, mnesia:dirty_write(Two)),
    ?match(ok, mnesia:dirty_write(Three)),
    ?match([One], mnesia:dirty_read({T1, 1})),
    ?match([_ | _], dump_registry(Node, T1)),

    NewOne = {T1, 1, 0, integer, 0, 1},
    NewFour = {T1, "4", 1, string, 4, "four"},

    ?match([NewOne], mnesia:dirty_read({T1, 1})),
    ?match([Two], mnesia:dirty_read({T1, "two"})),
    ?match([], mnesia:dirty_read({T1, 3})),
    ?match([NewFour], mnesia:dirty_read({T1, "4"})),

    T2 = blixt,
    ?match({'EXIT', {aborted, {no_exists, _}}},
	   mnesia:dirty_read({T2, 1})),
    ?match([_ |_], dump_registry(Node, T2)),

    NewOne2 = setelement(1, NewOne, T2),
    NewFour2 = setelement(1, NewFour, T2),

    ?match([NewOne2], mnesia:dirty_read({T2, 1})),
    ?match([], mnesia:dirty_read({T2, "two"})),
    ?match([], mnesia:dirty_read({T2, 3})),
    ?match([NewFour2], mnesia:dirty_read({T2, "4"})),
    ?match([_One2, NewFour2], lists:sort(restore_registry(Node, T2))),
    
    ?verify_mnesia(Nodes, []).

dump_registry(Node, Tab) ->
    case rpc:call(Node, mnesia_registry, start_dump, [Tab, self()]) of
	Pid when is_pid(Pid) ->
	    Pid ! {write, 1, 0, integer, 0, 1},
	    Pid ! {delete, 3},
	    Pid ! {write, "4", 1, string, 4, "four"},
	    Pid ! {commit, self()},
	    receive
		{ok, Pid} ->
		    [{Tab, "4", 1, string, 4, "four"},
		     {Tab, 1, 0, integer, 0, 1}];
		{'EXIT', Pid, Reason} ->
		    exit(Reason)
	    end;
	{badrpc, Reason} ->
	    exit(Reason)
    end.

restore_registry(Node, Tab) ->
    case rpc:call(Node, mnesia_registry, start_restore, [Tab, self()]) of
	{size, Pid, N, _LargestKeySize, _LargestValSize} ->
	    Pid ! {send_records, self()},
	    receive_records(Tab, N);
	{badrpc, Reason} ->
	    exit(Reason)
    end.

receive_records(Tab, N) when N > 0 ->
    receive
	{restore, KeySize, ValSize, ValType, Key, Val} -> 
	    [{Tab, Key, KeySize, ValType, ValSize, Val} | receive_records(Tab, N -1)];
	{'EXIT', _Pid, Reason} ->
	    exit(Reason)
    end;
receive_records(_Tab, 0) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bad_dump(doc) ->
    ["Intentionally fail with the dump of a faked C-node registry"];
bad_dump(suite) -> [];
bad_dump(Config) when is_list(Config) ->
    [Node] = Nodes = ?acquire_nodes(1, Config),
    
    OldTab = ming,
    ?match({'EXIT', {aborted, _}}, mnesia_registry:start_restore(no_tab, self())),
    ?match({atomic, ok}, mnesia:create_table(OldTab, [{attributes, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q]}])),
    ?match({'EXIT',{aborted,{bad_type,_}}}, dump_registry(Node, OldTab)),
    ?match(stopped, mnesia:stop()),

    ?match({'EXIT', {aborted, _}}, mnesia_registry:create_table(down_table)),
    ?match({'EXIT', {aborted, _}}, mnesia_registry:start_restore(no_tab, self())),
    ?match({'EXIT', {aborted, _}}, dump_registry(Node, down_dump)),

    ?verify_mnesia([], Nodes).

