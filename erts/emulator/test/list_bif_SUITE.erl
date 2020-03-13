%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2020. All Rights Reserved.
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

-module(list_bif_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).
-export([hd_test/1,tl_test/1,t_length/1,t_list_to_pid/1,
         t_list_to_ref/1, t_list_to_ext_pidportref/1,
         t_list_to_port/1,t_list_to_float/1,t_list_to_integer/1]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].


all() -> 
    [hd_test, tl_test, t_length, t_list_to_pid, t_list_to_port,
     t_list_to_ref, t_list_to_ext_pidportref,
     t_list_to_float, t_list_to_integer].

%% Tests list_to_integer and string:to_integer
t_list_to_integer(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch list_to_integer("12373281903728109372810937209817320981321ABC")),
    12373281903728109372810937209817320981321 = (catch list_to_integer("12373281903728109372810937209817320981321")),
    12373 = (catch list_to_integer("12373")),
    -12373 =  (catch list_to_integer("-12373")),
    12373 = (catch list_to_integer("+12373")),
    {'EXIT',{badarg,_}} = ( catch list_to_integer(abc)),
    {'EXIT',{badarg,_}} = (catch list_to_integer("")),
    {12373281903728109372810937209817320981321,"ABC"} = string:to_integer("12373281903728109372810937209817320981321ABC"),
    {-12373281903728109372810937209817320981321,"ABC"} = string:to_integer("-12373281903728109372810937209817320981321ABC"),
    {12,[345]} = string:to_integer([$1,$2,345]),
    {error, badarg} = string:to_integer([$1,$2,a]),
    {error,no_integer} = string:to_integer([$A]),
    {error,badarg} = string:to_integer($A),
    ok.

%% Test hd/1 with correct and incorrect arguments.
hd_test(Config) when is_list(Config) ->
    $h = hd(id("hejsan")),
    case catch hd(id($h)) of
        {'EXIT', {badarg, _}} -> ok;
        Res ->
            ct:fail("hd/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.


%% Test tl/1 with correct and incorrect arguments.
tl_test(Config) when is_list(Config) ->
    "ejsan" = tl(id("hejsan")),
    case catch tl(id(104)) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("tl/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.


%% Test length/1 with correct and incorrect arguments.

t_length(Config) when is_list(Config) ->
    0 = length(""),
    0 = length([]),
    1 = length([1]),
    2 = length([1,a]),
    2 = length("ab"),
    3 = length("abc"),
    4 = length(id([x|"abc"])),
    6 = length("hejsan"),
    {'EXIT',{badarg,_}} = (catch length(id([a,b|c]))),
    case catch length({tuple}) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("length/1 with incorrect args succeeded.~nResult: ~p", [Res])
    end,
    ok.
	      

%% Test list_to_pid/1 with correct and incorrect arguments.

t_list_to_pid(Config) when is_list(Config) ->
    Me = self(),
    MyListedPid = pid_to_list(Me),
    Me = list_to_pid(MyListedPid),
    case catch list_to_pid(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_pid/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

%% Test list_to_port/1 with correct and incorrect arguments.

t_list_to_port(Config) when is_list(Config) ->
    Me = hd(erlang:ports()),
    MyListedPid = port_to_list(Me),
    Me = list_to_port(MyListedPid),
    case catch list_to_port(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_port/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

t_list_to_ref(Config) when is_list(Config) ->
    Ref = make_ref(),
    RefStr = ref_to_list(Ref),
    Ref = list_to_ref(RefStr),
    case catch list_to_ref(id("Incorrect list")) of
        {'EXIT', {badarg, _}} ->
            ok;
        Res ->
            ct:fail("list_to_ref/1 with incorrect arg succeeded.~n"
                    "Result: ~p", [Res])
    end,
    ok.

%% Test list_to_pid/port/ref for external pids/ports/refs.
t_list_to_ext_pidportref(Config) when is_list(Config) ->
    {ok, Node} = slave:start(net_adm:localhost(), t_list_to_ext_pidportref),
    Pid = rpc:call(Node, erlang, self, []),
    Port = hd(rpc:call(Node, erlang, ports, [])),
    Ref = rpc:call(Node, erlang, make_ref, []),

    PidStr  = pid_to_list(Pid),
    PortStr = port_to_list(Port),
    RefStr  = ref_to_list(Ref),

    Pid2  = list_to_pid(PidStr),
    Port2 = list_to_port(PortStr),
    Ref2  = list_to_ref(RefStr),

    %% The local roundtrips of externals does not work
    %% as 'creation' is missing in the string formats and we don't know
    %% the 'creation' of the connected node.
    false = (Pid =:= Pid2),
    false = (Port =:= Port2),
    false = (Ref =:= Ref2),

    %% Local roundtrip kind of "works" for '==' since OTP-22.0 (bf7c722bd3b)
    %% Operator '==' treats 0-creations as wildcards
    %% which breaks term transitivity (A==B and B==C => B==C).
    true = (Pid == Pid2),
    true = (Port == Port2),
    true = (Ref == Ref2),

    %% It works when sent back to node with matching name, as 0-creations
    %% will be converted to the local node creation.
    true = rpc:call(Node, erlang, '=:=', [Pid, Pid2]),
    true = rpc:call(Node, erlang, '==',  [Pid, Pid2]),
    true = rpc:call(Node, erlang, '=:=', [Port, Port2]),
    true = rpc:call(Node, erlang, '==',  [Port, Port2]),
    true = rpc:call(Node, erlang, '=:=', [Ref, Ref2]),
    true = rpc:call(Node, erlang, '==',  [Ref, Ref2]),

    slave:stop(Node),
    ok.


%% Test list_to_float/1 with correct and incorrect arguments.

t_list_to_float(Config) when is_list(Config) ->
    5.89000 = list_to_float(id("5.89")),
    5.89898 = list_to_float(id("5.89898")),
    case catch list_to_float(id("58")) of
        {'EXIT', {badarg, _}} -> ok;
        Res ->
            ct:fail("list_to_float with incorrect arg succeeded.~nResult: ~p", [Res])
    end,
    ok.

id(I) -> I.
